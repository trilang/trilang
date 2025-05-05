using Tri.Tests.Builders;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class NotImplementedInterfaceTests
{
    [Test]
    public void EverythingIsImplementedInTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Interface1", builder => builder
                .Interface(i => i
                    .DefineProperty("x", "i32")
                    .DefineMethod("toString", m => m
                        .ReturnType("string"))))
            .DefineType("Test", builder => builder
                .AddInterface("Interface1")
                .DefineProperty("x", "i32")
                .DefineMethod("toString", m => m
                    .ReturnType("string")
                    .Body(body => body
                        .Return(r => r.String("Hello, World!")))))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.Nothing);
    }

    [Test]
    public void NotImplementedPropertyTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Interface1", builder => builder
                .Interface(i => i
                    .DefineProperty("x", "i32")
                    .DefineMethod("toString", m => m
                        .ReturnType("string"))))
            .DefineType("Test", builder => builder
                .AddInterface("Interface1")
                .DefineMethod("toString", m => m
                    .ReturnType("string")
                    .Body(body => body
                        .Return(r => r.String("Hello, World!")))))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'x' property is not implemented."));
    }

    [Test]
    public void ImplementPropertyWithIncorrectTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Interface1", builder => builder
                .Interface(i => i
                    .DefineProperty("x", "i32")
                    .DefineMethod("toString", m => m
                        .ReturnType("string"))))
            .DefineType("Test", builder => builder
                .AddInterface("Interface1")
                .DefineProperty("x", "i8")
                .DefineMethod("toString", m => m
                    .ReturnType("string")
                    .Body(body => body
                        .Return(r => r.String("Hello, World!")))))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'x' property is not of the correct type."));
    }

    [Test]
    public void NotImplementedMethodTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Interface1", builder => builder
                .Interface(i => i
                    .DefineProperty("x", "i32")
                    .DefineMethod("toString", m => m
                        .ReturnType("string"))))
            .DefineType("Test", builder => builder
                .AddInterface("Interface1")
                .DefineProperty("x", "i32"))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'toString' method is not implemented."));
    }

    [Test]
    public void ImplementMethodWithIncorrectReturnTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Interface1", builder => builder
                .Interface(i => i
                    .DefineProperty("x", "i32")
                    .DefineMethod("toString", m => m
                        .ReturnType("string"))))
            .DefineType("Test", builder => builder
                .AddInterface("Interface1")
                .DefineProperty("x", "i32")
                .DefineMethod("toString", m => m
                    .ReturnType("i32")
                    .Body(body => body
                        .Return(r => r.Number(1)))))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'toString' method is not of the correct type."));
    }

    [Test]
    public void ImplementMethodWithIncorrectParametersTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Interface1", builder => builder
                .Interface(i => i
                    .DefineProperty("x", "i32")
                    .DefineMethod("toString", m => m
                        .ReturnType("string"))))
            .DefineType("Test", builder => builder
                .AddInterface("Interface1")
                .DefineProperty("x", "i32")
                .DefineMethod("toString", m => m
                    .DefineParameter("a", "i32")
                    .ReturnType("string")
                    .Body(body => body
                        .Return(r => r.String("Hello, World!")))))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'toString' method is not of the correct type."));
    }
}