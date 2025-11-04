using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;
using Trilang.Semantics.Model;

namespace Tri.Tests.Semantics;

public class BreakContinueWithinLoopTests
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
    public void BreakIsNotInLoopTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(): void {
                break;
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0012BreakOutsideLoop,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(32, 2, 11))),
            "The 'break' keyword can only be used within a loop.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ContinueIsNotInLoopTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(): void {
                continue;
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0013ContinueOutsideLoop,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(35, 2, 14))),
            "The 'continue' keyword can only be used within a loop.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void BreakInNestedLoopTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(): void {
                while (true) {
                    while (false) {
                        break;
                    }
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var breakNode = semanticTree.Find<Break>();
        var loop = semanticTree.Where<While>().Last();
        Assert.That(breakNode, Is.Not.Null);
        Assert.That(breakNode.LoopNode, Is.EqualTo(loop));
    }

    [Test]
    public void ContinueInNestedLoopTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(): void {
                while (true) {
                    while (false) {
                        continue;
                    }
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var continueNode = semanticTree.Find<Continue>();
        var loop = semanticTree.Where<While>().Last();
        Assert.That(continueNode, Is.Not.Null);
        Assert.That(continueNode.LoopNode, Is.EqualTo(loop));
    }
}