using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class VariableUsedBeforeDeclaredTests
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
    public void VariableUsedAfterDeclarationTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var a: i32 = 1;
                a;
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
    public void ParameterUsedAfterDeclarationTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(a: i32): void {
                a;
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
    public void VariableUsedBeforeDeclarationTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                a;
                var a: i32 = 1;
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0025VariableUsedBeforeDeclaration,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(27, 2, 6))),
            "The 'a' variable is used before its declaration.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void VariableInBlockUsedBeforeDeclarationTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                {
                    a;
                }
                var a: i32 = 1;
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0025VariableUsedBeforeDeclaration,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(36, 3, 9), new SourcePosition(37, 3, 10))),
            "The 'a' variable is used before its declaration.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void VariableInDifferentBlocksTest()
    {
        var (tree, diagnostics) = Parse(
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
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0013UnknownMember,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(72, 6, 9), new SourcePosition(73, 6, 10))),
            "Unknown symbol: 'a'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void VariableInDeclaredInDifferentFunctionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(): void {
                var a: i32 = 1;
            }

            public main(): void {
                a;
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0013UnknownMember,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(71, 6, 5), new SourcePosition(72, 6, 6))),
            "Unknown symbol: 'a'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void VariableInParentScopeTest()
    {
        var (tree, diagnostics) = Parse(
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
            () => semantic.Analyze(
                [tree],
                new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.Nothing);
    }
}