using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class ThisOutsideOfTypeTests
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
    public void ThisInConstructorTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                public constructor() {
                    this;
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
    public void ThisInMethodTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                public toString(): void {
                    this;
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
    public void ThisInFunctionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                this;
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0022ThisOutsideOfType,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(30, 2, 9))),
            "The 'this' keyword can only be used within a type.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }
}