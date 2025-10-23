using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class PrivateInterfacePropertiesTests
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
    public void PrivateGetterTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test = {
                x: i32 { private get; }
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0027InterfacePropertyCantBePrivate,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(25, 2, 5), new SourcePosition(48, 2, 28))),
            "The getter of the interface property 'x' cannot be private.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void PrivateSetterTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test = {
                x: i32 { private set; }
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0027InterfacePropertyCantBePrivate,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(25, 2, 5), new SourcePosition(48, 2, 28))),
            "The setter of the interface property 'x' cannot be private.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void PrivateGetterAndSetterTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test = {
                x: i32 { private get; private set; }
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.S0027InterfacePropertyCantBePrivate,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    file,
                    new SourceSpan(new SourcePosition(25, 2, 5), new SourcePosition(61, 2, 41))),
                "The getter of the interface property 'x' cannot be private."),
            new Diagnostic(
                DiagnosticId.S0027InterfacePropertyCantBePrivate,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    file,
                    new SourceSpan(new SourcePosition(25, 2, 5), new SourcePosition(61, 2, 41))),
                "The setter of the interface property 'x' cannot be private.")
        };

        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }
}