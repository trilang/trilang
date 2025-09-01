using Trilang;
using Trilang.Lower;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;
using Trilang.Semantics.Passes.ControlFlow;

namespace Tri.Tests.Lower;

public class AddImplicitReturnStatementsTests
{
    private static (SyntaxTree, ControlFlowGraphMap) Parse(string code)
    {
        var parser = new Parser();
        var tree = parser.Parse(code);

        var semantic = new SemanticAnalysis();
        var (_, _, cfgs) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        return (tree, cfgs);
    }

    [Test]
    public void AddReturnToEmptyVoidMethodTest()
    {
        var (tree, cfgs) = Parse("function test(): void { }");

        var lowering = new Lowering();
        lowering.Lower(tree, new LoweringOptions([], cfgs));

        var function = tree.Find<FunctionDeclarationNode>()!;
        var expected = new BlockStatementNode([new ReturnStatementNode()]);

        Assert.That(function.Body, Is.EqualTo(expected));
    }

    [Test]
    public void AddReturnToEndAfterWhileTest()
    {
        var (tree, cfgs) = Parse(
            """
            function test(): void {
                while (true) { }
            }
            """);

        var lowering = new Lowering();
        lowering.Lower(tree, new LoweringOptions([], cfgs));

        var function = tree.Find<FunctionDeclarationNode>()!;
        var expected = new BlockStatementNode([
            new BlockStatementNode([
                new GoToNode("loop_0_start"),
                new LabelNode("loop_0_start"),
                new IfStatementNode(
                    new LiteralExpressionNode(LiteralExpressionKind.Boolean, true)
                    {
                        ReturnTypeMetadata = TypeMetadata.Bool
                    },
                    new BlockStatementNode([
                        new GoToNode("loop_0_start"),
                    ]),
                    new BlockStatementNode([
                        new GoToNode("loop_0_end"),
                    ])
                ),
                new LabelNode("loop_0_end"),
            ]),
            new ReturnStatementNode(),
        ]);

        Assert.That(function.Body, Is.EqualTo(expected));
    }

    [Test]
    public void AddReturnToEndAfterIfTest()
    {
        var (tree, cfgs) = Parse(
            """
            function test(): void {
                if (true) { }

                return;
            }
            """);

        var lowering = new Lowering();
        lowering.Lower(tree, new LoweringOptions([], cfgs));

        var function = tree.Find<FunctionDeclarationNode>()!;
        var expected = new BlockStatementNode([
            new IfStatementNode(
                new LiteralExpressionNode(LiteralExpressionKind.Boolean, true)
                {
                    ReturnTypeMetadata = TypeMetadata.Bool,
                },
                new BlockStatementNode([
                    new GoToNode("if_0_then"),
                ]),
                new BlockStatementNode([
                    new GoToNode("if_0_end"),
                ])
            ),
            new BlockStatementNode([
                new LabelNode("if_0_then"),
                new GoToNode("if_0_end"),
            ]),
            new LabelNode("if_0_end"),
            new ReturnStatementNode(),
        ]);

        Assert.That(function.Body, Is.EqualTo(expected));
    }

    [Test]
    public void KeepIfElseAsIsTest()
    {
        var (tree, cfgs) = Parse(
            """
            function test(): void {
                if (true) {
                    return;
                } else {
                    return;
                }
            }
            """);

        var lowering = new Lowering();
        lowering.Lower(tree, new LoweringOptions([], cfgs));

        var function = tree.Find<FunctionDeclarationNode>()!;
        var expected = new BlockStatementNode([
            new IfStatementNode(
                new LiteralExpressionNode(LiteralExpressionKind.Boolean, true)
                {
                    ReturnTypeMetadata = TypeMetadata.Bool
                },
                new BlockStatementNode([
                    new GoToNode("if_0_then"),
                ]),
                new BlockStatementNode([
                    new GoToNode("if_0_else"),
                ])
            ),
            new BlockStatementNode([
                new LabelNode("if_0_then"),
                new ReturnStatementNode(),
                new GoToNode("if_0_end"),
            ]),
            new BlockStatementNode([
                new LabelNode("if_0_else"),
                new ReturnStatementNode(),
                new GoToNode("if_0_end"),
            ]),
            new LabelNode("if_0_end"),
        ]);

        Assert.That(function.Body, Is.EqualTo(expected));
    }

    [Test]
    public void KeepInnerBlockAsIsTest()
    {
        var (tree, cfgs) = Parse(
            """
            function test(): void {
                {
                    return;
                }
            }
            """);

        var lowering = new Lowering();
        lowering.Lower(tree, new LoweringOptions([], cfgs));

        var function = tree.Find<FunctionDeclarationNode>()!;
        var expected = new BlockStatementNode([
            new BlockStatementNode([
                new ReturnStatementNode(),
            ]),
        ]);

        Assert.That(function.Body, Is.EqualTo(expected));
    }
}