using Tri.Tests.Builders;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class ThisOutsideOfClassTests
{
    [Test]
    public void ThisInConstructorTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .DefineConstructor(ctor => ctor
                    .Body(body => body
                        .Expression(exp => exp
                            .MemberAccess("this")))))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.Nothing);
    }

    [Test]
    public void ThisInMethodTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .DefineMethod("toString", method => method
                    .Body(body => body
                        .Expression(exp => exp
                            .MemberAccess("this")))))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.Nothing);
    }

    [Test]
    public void ThisInFunctionTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", f => f
                .Body(body => body
                    .Expression(exp => exp
                        .MemberAccess("this"))))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'this' keyword is only allowed inside a class."));
    }
}