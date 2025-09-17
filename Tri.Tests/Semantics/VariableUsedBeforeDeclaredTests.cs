using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class VariableUsedBeforeDeclaredTests
{
    private static SyntaxTree Parse(string code)
    {
        var parser = new Parser();
        var tree = parser.Parse(code);

        return tree;
    }

    [Test]
    public void VariableUsedAfterDeclarationTest()
    {
        var tree = Parse(
            """
            public main(): void {
                var a: i32 = 1;
                a;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.Nothing);
    }

    [Test]
    public void ParameterUsedAfterDeclarationTest()
    {
        var tree = Parse(
            """
            public main(a: i32): void {
                a;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.Nothing);
    }

    [Test]
    public void VariableUsedBeforeDeclarationTest()
    {
        var tree = Parse(
            """
            public main(): void {
                a;
                var a: i32 = 1;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'a' variable used before declaration."));
    }

    [Test]
    public void VariableInBlockUsedBeforeDeclarationTest()
    {
        var tree = Parse(
            """
            public main(): void {
                {
                    a;
                }
                var a: i32 = 1;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'a' variable used before declaration."));
    }

    [Test]
    public void VariableInDifferentBlocksTest()
    {
        var tree = Parse(
            """
            public main(): void {
                {
                    var a: i32 = 1;
                }
                {
                    a;
                }
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Unknown symbol: a"));
    }

    [Test]
    public void VariableInDeclaredInDifferentFunctionTest()
    {
        var tree = Parse(
            """
            public test(): void {
                var a: i32 = 1;
            }

            public main(): void {
                a;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Unknown symbol: a"));
    }

    [Test]
    public void VariableInParentScopeTest()
    {
        var tree = Parse(
            """
            public test(): i32 {
                var a: i32 = 1;
                {
                    return a;
                }
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.Nothing);
    }
}