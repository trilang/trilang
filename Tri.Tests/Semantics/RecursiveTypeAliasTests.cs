using Tri.Tests.Builders;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class RecursiveTypeAliasTests
{
    [Test]
    public void RecursiveTypeAliasTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Test", t => t.Type("Test"))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The recursive type alias detected: 'Test'."));
    }
}