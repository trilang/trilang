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

    private static SyntaxTree Parse(string code)
    {
        var diagnostics = new DiagnosticCollection();

        var lexer = new Lexer();
        var lexerOptions = new LexerOptions(new LexerDiagnosticReporter(diagnostics, file));
        var tokens = lexer.Tokenize(code, lexerOptions);

        var parser = new Parser();
        var parserOptions = new ParserOptions(file, new ParserDiagnosticReporter(diagnostics, file));
        var tree = parser.Parse(tokens, parserOptions);

        return tree;
    }

    [Test]
    public void BreakIsNotInLoopTest()
    {
        var tree = Parse(
            """
            public test(): void {
                break;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'break' keyword can only be used within a loop."));
    }

    [Test]
    public void ContinueIsNotInLoopTest()
    {
        var tree = Parse(
            """
            public test(): void {
                continue;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'continue' keyword can only be used within a loop."));
    }

    [Test]
    public void BreakInNestedLoopTest()
    {
        var tree = Parse(
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
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var breakNode = semanticTree.Find<Break>();
        var loop = semanticTree.Where<While>().Last();
        Assert.That(breakNode, Is.Not.Null);
        Assert.That(breakNode.LoopNode, Is.EqualTo(loop));
    }

    [Test]
    public void ContinueInNestedLoopTest()
    {
        var tree = Parse(
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
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var continueNode = semanticTree.Find<Continue>();
        var loop = semanticTree.Where<While>().Last();
        Assert.That(continueNode, Is.Not.Null);
        Assert.That(continueNode.LoopNode, Is.EqualTo(loop));
    }
}