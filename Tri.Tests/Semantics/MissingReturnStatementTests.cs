using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class MissingReturnStatementTests
{
    private static SyntaxTree Parse(string code)
    {
        var parser = new Parser();
        var tree = parser.Parse(code);

        return tree;
    }

    [Test]
    public void MissingReturnInFunctionTest()
    {
        var tree = Parse("function test(): i32 { }");

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("test: () => i32. Not all paths return a value."));
    }

    [Test]
    public void MissingReturnInFunctionWithIfTest()
    {
        var tree = Parse(
            """
            function test(): i32 {
                if (false) {
                    return 1;
                }
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("test: () => i32. Not all paths return a value."));
    }

    [Test]
    public void ValidReturnInFunctionWithIfTest()
    {
        var tree = Parse(
            """
            function test(): i32 {
                if (false) {
                    return 1;
                }

                return 0;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.Nothing);
    }

    [Test]
    public void ValidReturnInFunctionWithIfElseTest()
    {
        var tree = Parse(
            """
            function test(): i32 {
                if (false) {
                    return 1;
                } else {
                    return 0;
                }
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.Nothing);
    }

    [Test]
    public void MissingReturnInFunctionWithWhileTest()
    {
        var tree = Parse(
            """
            function test(): i32 {
                while (false) {
                    return 1;
                }
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("test: () => i32. Not all paths return a value."));
    }

    [Test]
    public void ValidReturnInFunctionWithWhileTest()
    {
        var tree = Parse(
            """
            function test(): i32 {
                while (false) { }

                return 0;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.Nothing);
    }

    [Test]
    public void ValidReturnInVoidFunctionTest()
    {
        var tree = Parse("function test(): void { }");

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.Nothing);
    }
}