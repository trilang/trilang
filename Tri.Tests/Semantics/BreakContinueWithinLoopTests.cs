using Trilang;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class BreakContinueWithinLoopTests
{
    private static SyntaxTree Parse(string code)
    {
        var parser = new Parser();
        var tree = parser.Parse(code);

        return tree;
    }

    [Test]
    public void BreakIsNotInLoopTest()
    {
        var tree = Parse(
            """
            function test(): void {
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
            function test(): void {
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
            function test(): void {
                while (true) {
                    while (false) {
                        break;
                    }
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var breakNode = tree.Find<BreakNode>();
        var loop = tree.Find<WhileNode>(x =>
            x.Condition.Equals(
                new LiteralExpressionNode(LiteralExpressionKind.Boolean, false)
                {
                    ReturnTypeMetadata = TypeMetadata.Bool
                }
            )
        );
        Assert.That(breakNode, Is.Not.Null);
        Assert.That(breakNode.LoopNode, Is.EqualTo(loop));
    }

    [Test]
    public void ContinueInNestedLoopTest()
    {
        var tree = Parse(
            """
            function test(): void {
                while (true) {
                    while (false) {
                        continue;
                    }
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var continueNode = tree.Find<ContinueNode>();
        var loop = tree.Find<WhileNode>(x =>
            x.Condition.Equals(
                new LiteralExpressionNode(LiteralExpressionKind.Boolean, false)
                {
                    ReturnTypeMetadata = TypeMetadata.Bool
                }
            )
        );
        Assert.That(continueNode, Is.Not.Null);
        Assert.That(continueNode.LoopNode, Is.EqualTo(loop));
    }
}