using Tri.Tests.Builders;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class CheckAccessModifiersTests
{
    [Test]
    public void PrivateCtorTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Test", t => t
                .DefineConstructor(ctor => ctor
                    .AccessModifier(AccessModifier.Private)))
            .DefineFunction("main", f => f
                .Body(body => body
                    .DefineVariable("x", "Test", exp => exp
                        .NewObject("Test"))))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The constructor of 'Test' is not accessible."));
    }

    [Test]
    public void IgnorePrivateCtorInTheSameTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Test", t => t
                .DefineConstructor(ctor => ctor
                    .AccessModifier(AccessModifier.Private))
                .DefineMethod("create", m => m
                    .ReturnType("Test")
                    .Body(body => body
                        .Return(r => r
                            .NewObject("Test")))))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.Nothing);
    }
}