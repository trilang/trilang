using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class MultifileSemanticTests
{
    private static SyntaxTree Parse(
        DiagnosticCollection diagnostics,
        string filePath,
        string code)
    {
        var file = new SourceFile(filePath);
        var lexer = new Lexer();
        var lexerOptions = new LexerOptions(new LexerDiagnosticReporter(diagnostics, file));
        var tokens = lexer.Tokenize(code, lexerOptions);

        var parser = new Parser();
        var parserOptions = new ParserOptions(file, new ParserDiagnosticReporter(diagnostics, file));
        var tree = parser.Parse(tokens, parserOptions);

        return tree;
    }

    [Test]
    public void UseTypeFromOtherFileTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(diagnostics, "point.tri", "public type Point {}");
        var file2 = Parse(diagnostics, "test.tri",
            """
            public main(): void {
                var p: Point = new Point();
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [file1, file2],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ForwardDeclareTypeFromOtherFileTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(diagnostics, "test.tri",
            """
            public main(): void {
                var p: Point = new Point();
            }
            """);
        var file2 = Parse(diagnostics, "point.tri", "public type Point {}");

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [file1, file2],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void DuplicateTypeDeclarationTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(diagnostics, "point1.tri", "public type Point {}");
        var file2 = Parse(diagnostics, "point2.tri", "public type Point {}");

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [file1, file2],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0002AlreadyDefined,
            DiagnosticSeverity.Error,
            new SourceLocation(
                new SourceFile("point2.tri"),
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(20, 1, 21))),
            "The 'Point' type is already defined.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }
}