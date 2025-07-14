using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class ThisOutsideOfClassTests
{
    private static SyntaxTree Parse(string code)
    {
        var parser = new Parser();
        var tree = parser.Parse(code);

        return tree;
    }

    [Test]
    public void ThisInConstructorTest()
    {
        var tree = Parse(
            """
            public type Point {
                public constructor() {
                    this;
                }
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.Nothing);
    }

    [Test]
    public void ThisInMethodTest()
    {
        var tree = Parse(
            """
            public type Point {
                public toString(): void {
                    this;
                }
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.Nothing);
    }

    [Test]
    public void ThisInFunctionTest()
    {
        var tree = Parse(
            """
            function main(): void {
                this;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'this' keyword is only allowed inside a type."));
    }
}