using Tri.Tests.Builders;
using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class TypeCheckerTests
{
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

        tree.Accept(new TypeChecker());

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

        tree.Accept(new TypeChecker());

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

        tree.Accept(new TypeChecker());

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

        tree.Accept(new TypeChecker());

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

        tree.Accept(new TypeChecker());

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

        tree.Accept(new TypeChecker());

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