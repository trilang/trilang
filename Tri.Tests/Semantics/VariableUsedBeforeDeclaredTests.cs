using Tri.Tests.Builders;
using Trilang.Parsing;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class VariableUsedBeforeDeclaredTests
{
    [Test]
    public void VariableUsedAfterDeclarationTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .DefineBody(body => body
                    .DefineVariable("a", "i32", exp => exp.Number(1))
                    .Statement(exp => exp.Variable("a"))))
            .Build();

        Assert.DoesNotThrow(() => tree.Accept(new VariableUsedBeforeDeclared(), new VisitorContext<object>()));
    }

    [Test]
    public void ParameterUsedAfterDeclarationTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .DefineParameter("a", "i32")
                .DefineBody(body => body
                    .Statement(exp => exp.Variable("a"))))
            .Build();

        Assert.DoesNotThrow(() => tree.Accept(new VariableUsedBeforeDeclared(), new VisitorContext<object>()));
    }

    [Test]
    public void VariableUsedBeforeDeclarationTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .DefineBody(body => body
                    .Statement(exp => exp.Variable("a"))
                    .DefineVariable("a", "i32", exp => exp.Number(1))))
            .Build();

        Assert.Throws<TypeCheckerException>(
            () => tree.Accept(new VariableUsedBeforeDeclared(), new VisitorContext<object>()));
    }

    [Test]
    public void VariableInBlockUsedBeforeDeclarationTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .DefineBody(body => body
                    .Block(block => block
                        .Statement(exp => exp.Variable("a")))
                    .DefineVariable("a", "i32", exp => exp.Number(1))))
            .Build();

        Assert.Throws<TypeCheckerException>(
            () => tree.Accept(new VariableUsedBeforeDeclared(), new VisitorContext<object>()));
    }

    [Test]
    public void VariableInDifferentBlocksTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .DefineBody(body => body
                    .Block(block => block.DefineVariable("a", "i32", exp => exp.Number(1)))
                    .Block(block => block.Statement(exp => exp.Variable("a")))))
            .Build();

        Assert.Throws<TypeCheckerException>(
            () => tree.Accept(new VariableUsedBeforeDeclared(), new VisitorContext<object>()));
    }

    [Test]
    public void VariableInDeclaredInDifferentFunctionTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("test", builder => builder
                .DefineBody(body => body
                    .DefineVariable("a", "i32", exp => exp.Number(1))))
            .DefineFunction("main", builder => builder
                .DefineBody(body => body
                    .Statement(exp => exp.Variable("a"))))
            .Build();

        Assert.Throws<TypeCheckerException>(
            () => tree.Accept(new VariableUsedBeforeDeclared(), new VisitorContext<object>()));
    }
}