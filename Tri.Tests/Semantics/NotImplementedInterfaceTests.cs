using Tri.Tests.Builders;
using Trilang.Parsing.Ast;
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

    [Test]
    public void TypeImplementsMethodAsPrivateTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Interface1", a => a
                .Interface(i => i
                    .DefineMethod("method", m => m.ReturnType("void"))))
            .DefineType("Test", t => t
                .AddInterface("Interface1")
                .DefineMethod("method", m => m.AccessModifier(AccessModifier.Private)))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The implementation of an interface method 'method' cannot be private."));
    }

    [Test]
    public void TypeImplementsGetterAsPrivateTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Interface1", a => a
                .Interface(i => i
                    .DefineProperty("x", "i32", AccessModifier.Public, AccessModifier.Public)))
            .DefineType("Test", t => t
                .AddInterface("Interface1")
                .DefineProperty("x", "i32", p => p
                    .Getter(AccessModifier.Private)
                    .Setter(AccessModifier.Public)))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The implementation of an interface property getter 'x' cannot be private."));
    }

    [Test]
    public void TypeImplementsSetterAsPrivateTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Interface1", a => a
                .Interface(i => i
                    .DefineProperty("x", "i32", AccessModifier.Public, AccessModifier.Public)))
            .DefineType("Test", t => t
                .AddInterface("Interface1")
                .DefineProperty("x", "i32", p => p
                    .Getter(AccessModifier.Public)
                    .Setter(AccessModifier.Private)))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The implementation of an interface property setter 'x' cannot be private."));
    }
}