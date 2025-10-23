using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class MissingReturnStatementTests
{
    private static readonly SourceFile file = new SourceFile("test.tri");

    private static (SyntaxTree, DiagnosticCollection) Parse(string code)
    {
        var diagnostics = new DiagnosticCollection();

        var lexer = new Lexer();
        var lexerOptions = new LexerOptions(new LexerDiagnosticReporter(diagnostics, file));
        var tokens = lexer.Tokenize(code, lexerOptions);

        var parser = new Parser();
        var parserOptions = new ParserOptions(file, new ParserDiagnosticReporter(diagnostics, file));
        var tree = parser.Parse(tokens, parserOptions);

        return (tree, diagnostics);
    }

    [Test]
    public void MissingReturnInFunctionTest()
    {
        var (tree, diagnostics) = Parse("public test(): i32 { }");

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0020NotAllPathsReturnValue,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(22, 1, 23))),
            "Not all paths return a value in 'test'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void MissingReturnInFunctionWithIfTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(): i32 {
                if (false) {
                    return 1;
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0020NotAllPathsReturnValue,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(63, 5, 2))),
            "Not all paths return a value in 'test'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ValidReturnInFunctionWithIfTest()
    {
        var (tree, diagnostics) = Parse(
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
            () => semantic.Analyze(
                [tree],
                new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.Nothing);
    }

    [Test]
    public void ValidReturnInFunctionWithIfElseTest()
    {
        var (tree, diagnostics) = Parse(
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
            () => semantic.Analyze(
                [tree],
                new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.Nothing);
    }

    [Test]
    public void MissingReturnInFunctionWithWhileTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(): i32 {
                while (false) {
                    return 1;
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0020NotAllPathsReturnValue,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(66, 5, 2))),
            "Not all paths return a value in 'test'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ValidReturnInFunctionWithWhileTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(): i32 {
                while (false) { }

                return 0;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(
                [tree],
                new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.Nothing);
    }

    [Test]
    public void ValidReturnInVoidFunctionTest()
    {
        var (tree, diagnostics) = Parse("public test(): void { }");

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(
                [tree],
                new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.Nothing);
    }
}