using Trilang;
using Trilang.Lower;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Lower;

public class AddValueParameterToSettersTests
{
    private static SyntaxTree Parse(string code)
    {
        var parser = new Parser();
        var tree = parser.Parse(code);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        return tree;
    }

    [Test]
    public void AddValueParameterToSetterTest()
    {
        const string code =
            """
            public type Test {
                count: i32;
            }
            """;
        var tree = Parse(code);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        var expected = new ParameterNode(MemberAccessExpressionNode.Value, new TypeNode("i32"));

        var setter = tree.Find<PropertySetterNode>();
        Assert.That(setter, Is.Not.Null);
        Assert.That(setter.Parameters, Has.Count.EqualTo(2));
        Assert.That(setter.Parameters, Has.One.EqualTo(expected));
    }
}