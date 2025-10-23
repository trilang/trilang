using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class ThisInStaticMethodsTests
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
    public void ThisInStaticMethodsTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test {
                public static test(): Test {
                    return this;
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0021ThisInStaticMethod,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(67, 3, 16), new SourcePosition(71, 3, 20))),
            "The 'this' keyword cannot be used in a static method.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }
}