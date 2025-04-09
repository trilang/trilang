using Tri.Tests.Builders;
using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Semantics;
using Trilang.Symbols;

namespace Tri.Tests.Semantics;

public class TypeCheckerTests
{
    private static T? Find<T>(ISyntaxNode? node)
        where T : class, ISyntaxNode
    {
        if (node is null)
            return null;

        if (node is T syntaxNode)
            return syntaxNode;

        return node switch
        {
            ArrayAccessExpressionNode arrayNode
                => Find<T>(arrayNode.Member) ??
                   Find<T>(arrayNode.Index),

            BinaryExpressionNode binaryExpressionNode
                => Find<T>(binaryExpressionNode.Left) ??
                   Find<T>(binaryExpressionNode.Right),

            BlockStatementNode blockStatementNode
                => blockStatementNode.Statements
                    .Select(Find<T>)
                    .FirstOrDefault(x => x is not null),

            BreakNode breakNode
                => null,

            CallExpressionNode callExpressionNode
                => callExpressionNode.Parameters
                    .Select(Find<T>)
                    .FirstOrDefault(x => x is not null),

            ContinueNode continueNode
                => null,

            ExpressionStatementNode expressionStatementNode
                => Find<T>(expressionStatementNode.Expression),

            FieldDeclarationNode fieldDeclarationNode
                => Find<T>(fieldDeclarationNode.Type),

            FunctionDeclarationNode functionDeclarationNode
                => functionDeclarationNode.Parameters
                       .Select(Find<T>)
                       .FirstOrDefault(x => x is not null) ??
                   Find<T>(functionDeclarationNode.Body),

            IfStatementNode ifStatementNode
                => Find<T>(ifStatementNode.Condition) ??
                   Find<T>(ifStatementNode.Then) ??
                   Find<T>(ifStatementNode.Else),

            LiteralExpressionNode
                => null,

            MethodDeclarationNode methodDeclarationNode
                => methodDeclarationNode.Parameters
                       .Select(Find<T>)
                       .FirstOrDefault(x => x is not null) ??
                   Find<T>(methodDeclarationNode.ReturnType) ??
                   Find<T>(methodDeclarationNode.Body),

            MemberAccessExpressionNode
                => null,

            ParameterNode
                => null,

            ReturnStatementNode returnStatementNode
                => Find<T>(returnStatementNode.Expression),

            SyntaxTree syntaxTree
                => syntaxTree.Declarations
                    .Select(Find<T>)
                    .FirstOrDefault(x => x is not null),

            TypeAliasNode typeAliasNode
                => Find<T>(typeAliasNode.Type),

            TypeDeclarationNode typeDeclarationNode
                => typeDeclarationNode.Fields
                       .Select(Find<T>)
                       .FirstOrDefault(x => x is not null) ??
                   typeDeclarationNode.Methods
                       .Select(Find<T>)
                       .FirstOrDefault(x => x is not null),

            TypeNode typeNode
                => null,

            VariableDeclarationStatementNode variableDeclarationStatementNode
                => Find<T>(variableDeclarationStatementNode.Expression),

            UnaryExpressionNode unaryExpressionNode
                => Find<T>(unaryExpressionNode.Operand),

            WhileNode whileNode
                => Find<T>(whileNode.Condition) ??
                   Find<T>(whileNode.Body),

            _ => throw new ArgumentOutOfRangeException(nameof(node)),
        };
    }

    [Test]
    public void SetMetadataForFunctionReturnTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .Body(_ => { }))
            .Build();

        tree.Accept(new TypeChecker());

        var expected = new FunctionMetadata("main", [], TypeMetadata.Void);
        var function = tree.Declarations[0] as FunctionDeclarationNode;
        Assert.That(function, Is.Not.Null);
        Assert.That(function.Metadata, Is.EqualTo(expected));
    }

    [Test]
    public void SetMetadataForIncorrectFunctionReturnTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("xxx")
                .Body(_ => { }))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker()));
    }

    [Test]
    public void SetMetadataForFunctionParameterTypesTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .DefineParameter("a", "i32")
                .DefineParameter("b", "bool")
                .Body(_ => { }))
            .Build();

        tree.Accept(new TypeChecker());

        var expected = new FunctionMetadata("main", [TypeMetadata.I32, TypeMetadata.Bool], TypeMetadata.Void);

        var function = tree.Declarations[0] as FunctionDeclarationNode;
        Assert.That(function, Is.Not.Null);
        Assert.That(function.Metadata, Is.EqualTo(expected));
        Assert.That(function.Parameters[0].Type.Metadata, Is.EqualTo(TypeMetadata.I32));
        Assert.That(function.Parameters[1].Type.Metadata, Is.EqualTo(TypeMetadata.Bool));
    }

    [Test]
    public void SetMetadataForIncorrectFunctionParameterTypesTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .DefineParameter("a", "i32")
                .DefineParameter("b", "xxx")
                .Body(_ => { }))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker()));
    }

    [Test]
    public void SetMetadataForVariableTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .Body(body => body
                    .DefineVariable("a", "i32", exp => exp.Number(1))))
            .Build();

        tree.Accept(new TypeChecker());

        var variable = Find<VariableDeclarationNode>(tree);
        Assert.That(variable, Is.Not.Null);
        Assert.That(variable.Type.Metadata, Is.EqualTo(TypeMetadata.I32));
    }

    [Test]
    public void SetMetadataForIncorrectVariableTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .Body(body => body
                    .DefineVariable("a", "xxx", exp => exp.Number(1))))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker()));
    }

    [Test]
    public void SetMetadataForTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .DefineField("x", "i32")
                .DefineField("y", "i32")
                .DefineMethod("toString", b => b.Body())
                .DefineMethod("distance", b => b
                    .DefineParameter("other", "i32")
                    .ReturnType("f64")
                    .Body()))
            .Build();

        var store = new TypeMetadataProvider();
        tree.Accept(new TypeChecker(store));

        var type = Find<TypeDeclarationNode>(tree);
        Assert.That(type, Is.Not.Null);
        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Types, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.Types, Contains.Key("Point").WithValue(TypeSymbol.Type("Point", type)));

        var metadata = store.GetType("Point");
        var expectedMetadata = new TypeMetadata(
            "Point",
            [
                new FieldMetadata(AccessModifierMetadata.Public, "x", TypeMetadata.I32),
                new FieldMetadata(AccessModifierMetadata.Public, "y", TypeMetadata.I32),
            ],
            [
                new MethodMetadata(AccessModifierMetadata.Public, "toString", [], TypeMetadata.Void),
                new MethodMetadata(
                    AccessModifierMetadata.Public,
                    "distance",
                    [new ParameterMetadata("other", TypeMetadata.I32)],
                    TypeMetadata.F64),
            ]);

        Assert.That(metadata, Is.EqualTo(expectedMetadata));
    }

    [Test]
    public void SetMetadataForAliasType()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("MyInt", "i32")
            .Build();

        var store = new TypeMetadataProvider();
        tree.Accept(new TypeChecker(store));

        var type = Find<TypeAliasNode>(tree);
        Assert.That(type, Is.Not.Null);
        Assert.That(tree.SymbolTable, Is.Not.Null);
        Assert.That(tree.SymbolTable.Types, Has.Count.EqualTo(1));
        Assert.That(tree.SymbolTable.Types, Contains.Key("MyInt").WithValue(TypeSymbol.Alias("MyInt", type)));

        var metadata = store.GetType("MyInt");
        var expectedMetadata = new TypeAliasMetadata("MyInt", TypeMetadata.I32);
        Assert.That(metadata, Is.EqualTo(expectedMetadata));
    }

    [Test]
    public void LiteralNumberTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("i32")
                .Body(body => body
                    .Return(exp => exp.Number(1))))
            .Build();

        tree.Accept(new TypeChecker());

        var returnNode = Find<ReturnStatementNode>(tree);
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(returnNode.Expression.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.I32));
    }

    [Test]
    public void LiteralBoolTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("bool")
                .Body(body => body
                    .Return(exp => exp.True())))
            .Build();

        tree.Accept(new TypeChecker());

        var returnNode = Find<ReturnStatementNode>(tree);
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(returnNode.Expression.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.Bool));
    }

    [Test]
    public void LiteralCharTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("char")
                .Body(body => body
                    .Return(exp => exp.Char('x'))))
            .Build();

        tree.Accept(new TypeChecker());

        var returnNode = Find<ReturnStatementNode>(tree);
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(returnNode.Expression.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.Char));
    }

    [Test]
    public void LiteralStringTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("string")
                .Body(body => body
                    .Return(exp => exp.String("xxx"))))
            .Build();

        tree.Accept(new TypeChecker());

        var returnNode = Find<ReturnStatementNode>(tree);
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(returnNode.Expression.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.String));
    }

    [Test]
    public void ReturnStatementDoesntTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("bool")
                .Body(body => body
                    .Return(exp => exp.Number(1))))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker()));
    }

    [Test]
    public void UnaryPlusTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("i32")
                .Body(body => body
                    .Return(exp => exp.Number(1).UnaryMinus())))
            .Build();

        tree.Accept(new TypeChecker());

        var returnNode = Find<ReturnStatementNode>(tree);
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(returnNode.Expression.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.I32));
    }

    [Test]
    public void UnaryMinusTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("i32")
                .Body(body => body
                    .Return(exp => exp.Number(1).UnaryMinus())))
            .Build();

        tree.Accept(new TypeChecker());

        var returnNode = Find<ReturnStatementNode>(tree);
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(returnNode.Expression.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.I32));
    }

    [Test]
    public void LogicalNotTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("bool")
                .Body(body => body
                    .Return(exp => exp.True().LogicalNot())))
            .Build();

        tree.Accept(new TypeChecker());

        var returnNode = Find<ReturnStatementNode>(tree);
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(returnNode.Expression.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.Bool));
    }

    [Test]
    public void BinaryExpressionTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("i32")
                .Body(body => body
                    .Return(exp => exp.Number(1).Number(2).Add())))
            .Build();

        tree.Accept(new TypeChecker());

        var binaryNode = Find<BinaryExpressionNode>(tree);
        Assert.That(binaryNode, Is.Not.Null);
        Assert.That(binaryNode.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.I32));
    }

    [Test]
    public void LogicalNotIncorrectOperandTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("i32")
                .Body(body => body
                    .Return(exp => exp.Number(1).LogicalNot())))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker()));
    }

    [Test]
    public void VariableExpressionTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .DefineParameter("a", "i32")
                .ReturnType("i32")
                .Body(body => body
                    .Return(exp => exp.Variable("a"))))
            .Build();

        tree.Accept(new TypeChecker());

        var returnNode = Find<ReturnStatementNode>(tree);
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(returnNode.Expression.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.I32));
    }

    [Test]
    public void VariableDeclarationIncorrectTypesTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .Body(body => body
                    .DefineVariable("a", "i32", exp => exp.True())))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker()));
    }

    [Test]
    public void IfIncorrectConditionTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .Body(body => body
                    .If(exp => exp.Number(1), _ => { })))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker()));
    }

    [Test]
    public void FunctionCallIncorrectParameterTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("add", builder => builder
                .DefineParameter("a", "i32")
                .ReturnType("i32")
                .Body(_ => { }))
            .DefineFunction("main", builder => builder
                .ReturnType("i32")
                .Body(body => body
                    .Return(exp => exp.True().Call("add"))))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker()));
    }

    [Test]
    public void WhileNonBoolConditionTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .Body(body => body
                    .While(exp => exp.Number(1), _ => { })))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker()));
    }

    [Test]
    public void ReturnInConstructorTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .DefineConstructor(ctor => ctor
                    .Body(body => body
                        .Return())))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker()));
    }

    [Test]
    public void ReturnWithExpressionInConstructorTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .DefineConstructor(ctor => ctor
                    .Body(body => body
                        .Return(exp => exp.Number(0)))))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker()));
    }
}