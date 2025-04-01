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
            CallExpressionNode callExpressionNode
                => callExpressionNode.Parameters
                    .Select(Find<T>)
                    .FirstOrDefault(x => x is not null),

            BinaryExpressionNode binaryExpressionNode
                => Find<T>(binaryExpressionNode.Left) ??
                   Find<T>(binaryExpressionNode.Right),

            LiteralExpressionNode
                => null,

            UnaryExpressionNode unaryExpressionNode
                => Find<T>(unaryExpressionNode.Operand),

            VariableExpressionNode
                => null,

            BlockStatementNode blockStatementNode
                => blockStatementNode.Statements
                    .Select(Find<T>)
                    .FirstOrDefault(x => x is not null),

            ExpressionStatementNode expressionStatementNode
                => Find<T>(expressionStatementNode.Expression),

            FunctionDeclarationNode functionDeclarationNode
                => functionDeclarationNode.Parameters
                       .Select(Find<T>)
                       .FirstOrDefault(x => x is not null) ??
                   Find<T>(functionDeclarationNode.Body),

            FunctionParameterNode
                => null,

            IfStatementNode ifStatementNode
                => Find<T>(ifStatementNode.Condition) ??
                   Find<T>(ifStatementNode.Then) ??
                   Find<T>(ifStatementNode.Else),

            ReturnStatementNode returnStatementNode
                => Find<T>(returnStatementNode.Expression),

            VariableDeclarationStatementNode variableDeclarationStatementNode
                => Find<T>(variableDeclarationStatementNode.Expression),

            SyntaxTree syntaxTree
                => syntaxTree.Functions
                    .Select(Find<T>)
                    .FirstOrDefault(x => x is not null),

            _ => throw new ArgumentOutOfRangeException(nameof(node)),
        };
    }

    [Test]
    public void MatchFunctionReturnTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .DefineBody(_ => { }))
            .Build();

        tree.Accept(new TypeChecker());

        var expected = new FunctionMetadata([], TypeMetadata.Void);
        Assert.That(tree.Functions[0].Metadata, Is.EqualTo(expected));
    }

    [Test]
    public void MatchIncorrectFunctionReturnTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("xxx")
                .DefineBody(_ => { }))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker()));
    }

    [Test]
    public void MatchFunctionParameterTypesTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .DefineParameter("a", "i32")
                .DefineParameter("b", "bool")
                .DefineBody(_ => { }))
            .Build();

        tree.Accept(new TypeChecker());

        var expected = new FunctionMetadata([TypeMetadata.I32, TypeMetadata.Bool], TypeMetadata.Void);

        var function = tree.Functions[0];
        Assert.That(function.Metadata, Is.EqualTo(expected));
        Assert.That(function.Parameters[0].TypeMetadata, Is.EqualTo(TypeMetadata.I32));
        Assert.That(function.Parameters[1].TypeMetadata, Is.EqualTo(TypeMetadata.Bool));
    }

    [Test]
    public void MatchIncorrectFunctionParameterTypesTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .DefineParameter("a", "i32")
                .DefineParameter("b", "xxx")
                .DefineBody(_ => { }))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker()));
    }

    [Test]
    public void VariableTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .DefineBody(body => body
                    .DefineVariable("a", "i32", exp => exp.Number(1))))
            .Build();

        tree.Accept(new TypeChecker());

        var variable = Find<VariableDeclarationNode>(tree);
        Assert.That(variable, Is.Not.Null);
        Assert.That(variable.TypeMetadata, Is.EqualTo(TypeMetadata.I32));
    }

    [Test]
    public void IncorrectVariableTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .DefineBody(body => body
                    .DefineVariable("a", "xxx", exp => exp.Number(1))))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker()));
    }

    [Test]
    public void LiteralNumberTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("i32")
                .DefineBody(body => body
                    .Return(exp => exp.Number(1))))
            .Build();

        tree.Accept(new TypeChecker());

        var returnNode = Find<ReturnStatementNode>(tree);
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.I32));
    }

    [Test]
    public void LiteralBoolTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("bool")
                .DefineBody(body => body
                    .Return(exp => exp.True())))
            .Build();

        tree.Accept(new TypeChecker());

        var returnNode = Find<ReturnStatementNode>(tree);
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.Bool));
    }

    [Test]
    public void LiteralCharTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("char")
                .DefineBody(body => body
                    .Return(exp => exp.Char('x'))))
            .Build();

        tree.Accept(new TypeChecker());

        var returnNode = Find<ReturnStatementNode>(tree);
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.Char));
    }

    [Test]
    public void LiteralStringTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("string")
                .DefineBody(body => body
                    .Return(exp => exp.String("xxx"))))
            .Build();

        tree.Accept(new TypeChecker());

        var returnNode = Find<ReturnStatementNode>(tree);
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.String));
    }

    [Test]
    public void ReturnStatementDoesntTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("bool")
                .DefineBody(body => body
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
                .DefineBody(body => body
                    .Return(exp => exp.Number(1).UnaryMinus())))
            .Build();

        tree.Accept(new TypeChecker());

        var returnNode = Find<ReturnStatementNode>(tree);
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.I32));
    }

    [Test]
    public void UnaryMinusTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("i32")
                .DefineBody(body => body
                    .Return(exp => exp.Number(1).UnaryMinus())))
            .Build();

        tree.Accept(new TypeChecker());

        var returnNode = Find<ReturnStatementNode>(tree);
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.I32));
    }

    [Test]
    public void LogicalNotTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("bool")
                .DefineBody(body => body
                    .Return(exp => exp.True().LogicalNot())))
            .Build();

        tree.Accept(new TypeChecker());

        var returnNode = Find<ReturnStatementNode>(tree);
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.Bool));
    }

    [Test]
    public void BinaryExpressionTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .ReturnType("i32")
                .DefineBody(body => body
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
                .DefineBody(body => body
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
                .DefineBody(body => body
                    .Return(exp => exp.Variable("a"))))
            .Build();

        tree.Accept(new TypeChecker());

        var returnNode = Find<ReturnStatementNode>(tree);
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.I32));
    }

    [Test]
    public void VariableDeclarationIncorrectTypesTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .DefineBody(body => body
                    .DefineVariable("a", "i32", exp => exp.True())))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker()));
    }

    [Test]
    public void IfIncorrectConditionTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .DefineBody(body => body
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
                .DefineBody(_ => { }))
            .DefineFunction("main", builder => builder
                .ReturnType("i32")
                .DefineBody(body => body
                    .Return(exp => exp.True().Call("add"))))
            .Build();

        Assert.Throws<TypeCheckerException>(() => tree.Accept(new TypeChecker()));
    }
}