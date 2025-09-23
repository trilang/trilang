using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class MissingReturnStatementTests
{
    private static SyntaxTree Parse(string code)
    {
        var diagnostics = new DiagnosticCollection();

        var lexer = new Lexer();
        var tokens = lexer.Tokenize(code, new LexerOptions(diagnostics.Lexer));

        var parser = new Parser();
        var tree = parser.Parse(tokens, new ParserOptions(diagnostics.Parser));

        return tree;
    }

    [Test]
    public void MissingReturnInFunctionTest()
    {
        var tree = Parse("public test(): i32 { }");

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
            public test(): i32 {
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
            public test(): i32 {
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
            public test(): i32 {
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
            public test(): i32 {
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
            public test(): i32 {
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
        var tree = Parse("public test(): void { }");

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.Nothing);
    }
}