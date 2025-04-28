using Tri.Tests.Builders;
using Trilang;
using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class TypeCheckerTests
{
    [Test]
    public void SetMetadataForFunctionReturnTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .Body(_ => { }))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expected = new FunctionMetadata("main", new FunctionTypeMetadata([], TypeMetadata.Void));
        var function = tree.Find<FunctionDeclarationNode>();
        Assert.That(function, Is.Not.Null);
        Assert.That(function.Metadata, Is.EqualTo(expected));
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

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expected = new FunctionMetadata(
            "main",
            new FunctionTypeMetadata([TypeMetadata.I32, TypeMetadata.Bool], TypeMetadata.Void));

        var function = tree.Find<FunctionDeclarationNode>();
        Assert.That(function, Is.Not.Null);
        Assert.That(function.Metadata, Is.EqualTo(expected));
        Assert.That(function.Parameters[0].Type.Metadata, Is.EqualTo(TypeMetadata.I32));
        Assert.That(function.Parameters[1].Type.Metadata, Is.EqualTo(TypeMetadata.Bool));
    }

    [Test]
    public void SetMetadataForVariableTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .Body(body => body
                    .DefineVariable("a", "i32", exp => exp.Number(1))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var variable = tree.Find<VariableDeclarationStatementNode>();
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

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Referenced unknown type 'xxx'"));
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

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expected = new TypeMetadata("Point", [], [], []);
        expected.AddField(new FieldMetadata(
            expected,
            AccessModifierMetadata.Public,
            "x",
            TypeMetadata.I32));
        expected.AddField(new FieldMetadata(
            expected,
            AccessModifierMetadata.Public,
            "y",
            TypeMetadata.I32));
        expected.AddMethod(new MethodMetadata(
            expected,
            AccessModifierMetadata.Public,
            "toString",
            new FunctionTypeMetadata([], TypeMetadata.Void)));
        expected.AddMethod(new MethodMetadata(
            expected,
            AccessModifierMetadata.Public,
            "distance",
            new FunctionTypeMetadata([TypeMetadata.I32], TypeMetadata.F64)));

        var type = tree.Find<TypeDeclarationNode>();
        Assert.That(type, Is.Not.Null);
        Assert.That(type.Metadata, Is.EqualTo(expected));
    }

    [Test]
    public void SetMetadataForAliasType()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("MyInt", new TypeNode("i32"))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expected = new TypeAliasMetadata("MyInt", TypeMetadata.I32);
        var node = tree.Find<TypeAliasDeclarationNode>();
        Assert.That(node, Is.Not.Null);
        Assert.That(node.Metadata, Is.EqualTo(expected));
    }

    [Test]
    public void SetMetadataForFunctionTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("MyF", builder => builder
                .DefineFunctionType(f => f
                    .DefineParameter("i32")
                    .DefineParameter("bool")
                    .ReturnType("f64")))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expected = new FunctionTypeMetadata([TypeMetadata.I32, TypeMetadata.Bool], TypeMetadata.F64);
        var type = tree.Find<FunctionTypeNode>();
        Assert.That(type, Is.Not.Null);
        Assert.That(type.Metadata, Is.EqualTo(expected));
    }

    [Test]
    public void SetMetadataForFunctionTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("add", builder => builder
                .DefineParameter("a", "i32")
                .DefineParameter("b", "i32")
                .ReturnType("i32")
                .Body(body => body.Return(exp => exp.Number(0))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expected = new FunctionMetadata(
            "add",
            new FunctionTypeMetadata([TypeMetadata.I32, TypeMetadata.I32], TypeMetadata.I32));

        var node = tree.Find<FunctionDeclarationNode>();
        Assert.That(node, Is.Not.Null);
        Assert.That(node.Metadata, Is.EqualTo(expected));
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

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var returnNode = tree.Find<ReturnStatementNode>();
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

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var returnNode = tree.Find<ReturnStatementNode>();
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

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var returnNode = tree.Find<ReturnStatementNode>();
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

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var returnNode = tree.Find<ReturnStatementNode>();
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

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Function return type mismatch: expected 'bool', got 'i32'"));
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

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var returnNode = tree.Find<ReturnStatementNode>();
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

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var returnNode = tree.Find<ReturnStatementNode>();
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

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var returnNode = tree.Find<ReturnStatementNode>();
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

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var binaryNode = tree.Find<BinaryExpressionNode>();
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

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Invalid unary expression: incompatible operand type 'i32' for operator 'LogicalNot'"));
    }

    [Test]
    public void VariableExpressionTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .DefineParameter("a", "i32")
                .ReturnType("i32")
                .Body(body => body
                    .Return(exp => exp.MemberAccess("a"))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var returnNode = tree.Find<ReturnStatementNode>();
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

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Type mismatch in variable declaration 'a': expected 'i32', got 'bool'"));
    }

    [Test]
    public void IfIncorrectConditionTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .Body(body => body
                    .If(exp => exp.Number(1), _ => { })))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The condition returns non-boolean type."));
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

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Expected 'i32' but got 'bool'"));
    }

    [Test]
    public void WhileNonBoolConditionTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .Body(body => body
                    .While(exp => exp.Number(1), _ => { })))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Condition must be a boolean"));
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

        var semantic = new SemanticAnalysis();

        Assert.That(() => semantic.Analyze(tree), Throws.Nothing);
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

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Constructor return type mismatch: expected 'void', got 'i32'"));
    }

    [Test]
    public void SetMetadataForInterfaceTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Point", builder => builder
                .DefineInterface(i => i
                    .DefineField("x", "i32")
                    .DefineField("y", "i32")
                    .DefineMethod("distance", m => m
                        .DefineParameter("other", "Point")
                        .ReturnType("f64"))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var interfaceType = new InterfaceMetadata("{ x: i32; y: i32; distance(Point): f64; }");
        interfaceType.AddField(new InterfaceFieldMetadata(interfaceType, "x", TypeMetadata.I32));
        interfaceType.AddField(new InterfaceFieldMetadata(interfaceType, "y", TypeMetadata.I32));
        interfaceType.AddMethod(
            new InterfaceMethodMetadata(
                interfaceType,
                "distance",
                new FunctionTypeMetadata([TypeMetadata.I32], TypeMetadata.F64)));
        var expected = new TypeAliasMetadata("Point", interfaceType);

        var type = tree.Find<TypeAliasDeclarationNode>();
        Assert.That(type, Is.Not.Null);
        Assert.That(type.Metadata, Is.EqualTo(expected));
    }

    [Test]
    public void SetReturnTypeForVariableWithFunctionTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("add", builder => builder
                .DefineParameter("a", "i32")
                .DefineParameter("b", "i32")
                .ReturnType("i32"))
            .DefineFunction("main", builder => builder
                .Body(body => body
                    .DefineVariable(
                        "x",
                        new FunctionTypeNode([new TypeNode("i32"), new TypeNode("i32")], new TypeNode("i32")),
                        v => v.MemberAccess("add"))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expected = new FunctionTypeMetadata([TypeMetadata.I32, TypeMetadata.I32], TypeMetadata.I32);
        var memberAccess = tree.Find<MemberAccessExpressionNode>();
        Assert.That(memberAccess, Is.Not.Null);
        Assert.That(memberAccess.ReturnTypeMetadata, Is.EqualTo(expected));
    }

    [Test]
    public void CallNonFunctionTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("test", builder => builder
                .DefineParameter("a", "i32")
                .Body(body => body
                    .Expression(r => r
                        .Call("a"))))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Cannot call a non-function member"));
    }

    [Test]
    public void ThisReturnTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .DefineMethod("toString", b => b
                    .Body(body => body
                        .Expression(exp => exp
                            .MemberAccess("this")))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var thisNode = tree.Find<MemberAccessExpressionNode>(m => m.Name == "this");
        var pointType = semantic.TypeProvider.GetType("Point");
        Assert.That(thisNode, Is.Not.Null);
        Assert.That(thisNode.ReturnTypeMetadata, Is.EqualTo(pointType));
    }

    [Test]
    public void ThisWithMultipleMembersReturnTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .DefineField("a", "i32")
                .DefineMethod("toString", b => b
                    .Body(body => body
                        .Expression(exp => exp
                            .MemberAccess("this")
                            .MemberAccess("a")))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var thisNode = tree.Find<MemberAccessExpressionNode>(m => m.Name == "a");
        Assert.That(thisNode, Is.Not.Null);
        Assert.That(thisNode.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.I32));
    }

    [Test]
    public void ThisWithIncorrectFieldNameTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .DefineField("a", "i32")
                .DefineMethod("toString", b => b
                    .Body(body => body
                        .Expression(exp => exp
                            .MemberAccess("this")
                            .MemberAccess("x")))))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Cannot find member 'x' in type 'Point'"));
    }

    [Test]
    public void InterfaceMemberAccessReturnTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Point", builder => builder
                .DefineInterface(i => i
                    .DefineField("x", "i32")))
            .DefineFunction("test", builder => builder
                .DefineParameter("a", new TypeNode("Point"))
                .ReturnType("i32")
                .Body(body => body
                    .Return(r => r
                        .MemberAccess("a")
                        .MemberAccess("x"))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var aNode = tree.Find<MemberAccessExpressionNode>(m => m.Name == "a");
        var pointType = semantic.TypeProvider.GetType("Point");
        Assert.That(aNode, Is.Not.Null);
        Assert.That(aNode.ReturnTypeMetadata, Is.EqualTo(pointType));

        var xNode = tree.Find<MemberAccessExpressionNode>(m => m.Name == "x");
        Assert.That(xNode, Is.Not.Null);
        Assert.That(xNode.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.I32));
    }

    [Test]
    public void InterfaceMemberAccessIncorrectFieldTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Point", builder => builder
                .DefineInterface(i => i
                    .DefineField("x", "i32")))
            .DefineFunction("test", builder => builder
                .DefineParameter("a", new TypeNode("Point"))
                .ReturnType("i32")
                .Body(body => body
                    .Return(r => r
                        .MemberAccess("a")
                        .MemberAccess("c"))))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Cannot find member 'c' in interface 'Point'"));
    }

    [Test]
    public void AliasFunctionTypeMemberAccessReturnTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("F", builder => builder
                .DefineFunctionType(f => f
                    .ReturnType("void")))
            .DefineType("Test", builder => builder
                .DefineField("f", new TypeNode("F")))
            .DefineFunction("test", builder => builder
                .DefineParameter("a", new TypeNode("Test"))
                .ReturnType("F")
                .Body(body => body
                    .Return(r => r
                        .MemberAccess("a")
                        .MemberAccess("f"))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var aNode = tree.Find<MemberAccessExpressionNode>(m => m.Name == "a");
        var pointType = semantic.TypeProvider.GetType("Test");
        Assert.That(aNode, Is.Not.Null);
        Assert.That(aNode.ReturnTypeMetadata, Is.EqualTo(pointType));

        var xNode = tree.Find<MemberAccessExpressionNode>(m => m.Name == "f");
        var functionType = semantic.TypeProvider.GetType("F");
        Assert.That(xNode, Is.Not.Null);
        Assert.That(xNode.ReturnTypeMetadata, Is.EqualTo(functionType));
    }

    [Test]
    public void NewOperatorSetCtorTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .DefineConstructor(c => c
                    .DefineParameter("x", "i32")
                    .DefineParameter("y", "i32")))
            .DefineFunction("test", builder => builder
                .Body(body => body
                    .DefineVariable("a", new TypeNode("Point"), exp => exp
                        .Number(1)
                        .Number(2)
                        .New("Point"))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var type = semantic.TypeProvider.GetType("Point") as TypeMetadata;
        Assert.That(type, Is.Not.Null);

        var ctor = type.GetConstructor([TypeMetadata.I32, TypeMetadata.I32]);
        Assert.That(ctor, Is.Not.Null);

        var newOp = tree.Find<NewExpressionNode>();
        Assert.That(newOp, Is.Not.Null);
        Assert.That(newOp.Metadata, Is.EqualTo(ctor));
    }

    [Test]
    public void NewOperatorForInterfaceTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Point", builder => builder
                .DefineInterface(i => i
                    .DefineField("x", "i32")
                    .DefineField("y", "i32")))
            .DefineFunction("test", builder => builder
                .Body(body => body
                    .DefineVariable("a", new TypeNode("Point"), exp => exp
                        .Number(1)
                        .Number(2)
                        .New("Point"))))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Cannot create an instance of type 'Point'"));
    }

    [Test]
    public void NewOperatorMissingConstructorTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .DefineConstructor(c => c
                    .DefineParameter("x", "i32")
                    .DefineParameter("y", "i32")))
            .DefineFunction("test", builder => builder
                .Body(body => body
                    .DefineVariable("a", new TypeNode("Point"), exp => exp
                        .Number(2)
                        .New("Point"))))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'Point' type doesn't have 'i32' constructor."));
    }

    [Test]
    public void SetMetadataForDiscriminatedUnionTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("DU", builder => builder
                .DefineDiscriminatedUnion(du => du
                    .AddInterface()
                    .AddType("i32")
                    .AddFunctionType(f => f.ReturnType("void"))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var du = new DiscriminatedUnionType("{ } | i32 | () => void");
        du.AddType(new InterfaceMetadata("{ }", [], []));
        du.AddType(TypeMetadata.I32);
        du.AddType(new FunctionTypeMetadata([], TypeMetadata.Void));
        var alias = new TypeAliasMetadata("DU", du);

        var aliasNode = tree.Find<TypeAliasDeclarationNode>();
        Assert.That(aliasNode, Is.Not.Null);
        Assert.That(aliasNode.Metadata, Is.EqualTo(alias));

        var duNode = tree.Find<DiscriminatedUnionNode>();
        Assert.That(duNode, Is.Not.Null);
        Assert.That(duNode.Metadata, Is.EqualTo(du));
    }
}