using Tri.Tests.Builders;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class BreakContinueWithinLoopTests
{
    [Test]
    public void BreakIsNotInLoopTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("test", builder => builder
                .Body(body => body
                    .Break()))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'break' keyword can only be used within a loop."));
    }

    [Test]
    public void ContinueIsNotInLoopTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("test", builder => builder
                .Body(body => body
                    .Continue()))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'continue' keyword can only be used within a loop."));
    }
}