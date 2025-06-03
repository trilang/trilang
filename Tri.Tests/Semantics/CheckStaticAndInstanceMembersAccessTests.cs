using Tri.Tests.Builders;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class CheckStaticAndInstanceMembersAccessTests
{
    [Test]
    public void AccessNotStaticMethodOnTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Test", t => t
                .DefineMethod("s", m => m.Body()))
            .DefineFunction("func", f => f
                .Body(body => body
                    .Expression(e => e
                        .MemberAccess("Test")
                        .MemberAccess("s")
                        .Call())))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The instance method 's' cannot be called on a static one."));
    }

    [Test]
    public void AccessStaticMethodOnInstanceTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Test", t => t
                .DefineMethod("s", m => m.Static()))
            .DefineFunction("func", f => f
                .DefineParameter("a", t => t.Type("Test"))
                .Body(body => body
                    .Expression(e => e
                        .MemberAccess("a")
                        .MemberAccess("s")
                        .Call())))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The static method 's' cannot be called on an instance one."));
    }

    [Test]
    public void AccessMemberOnInterfaceTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Test", a => a
                .Interface(i => i
                    .DefineMethod("s")))
            .DefineFunction("func", f => f
                .Body(body => body
                    .Expression(e => e
                        .MemberAccess("Test")
                        .MemberAccess("s")
                        .Call())))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("'Test' can't be used to call static members."));
    }
}