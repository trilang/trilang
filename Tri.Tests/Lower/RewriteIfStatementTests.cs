using Trilang.Lower;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Lower;

public class RewriteIfStatementTests
{
    private static SyntaxTree Parse(string code)
    {
        var parser = new Parser();
        var tree = parser.Parse(code);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        return tree;
    }

    [Test]
    public void RewriteIfElseStatementTest()
    {
        var tree = Parse(
            """
            function test(a: i32): i32 {
                if (a >= 0) {
                    return a;
                } else {
                    return -a;
                }
            }
            """);
        var expected = new SyntaxTree([
            new FunctionDeclarationNode(
                "test",
                [new ParameterNode("a", new TypeNode("i32"))],
                new TypeNode("i32"),
                new BlockStatementNode([
                    new IfStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.GreaterThanOrEqual,
                            new MemberAccessExpressionNode("a"),
                            LiteralExpressionNode.Number(0)
                        ),
                        new BlockStatementNode([
                            new GoToNode("if_0_then")
                        ]),
                        new BlockStatementNode([
                            new GoToNode("if_0_else")
                        ])
                    ),
                    new BlockStatementNode([
                        new LabelNode("if_0_then"),
                        new ReturnStatementNode(
                            new MemberAccessExpressionNode("a")
                        ),
                        new GoToNode("if_0_end"),
                    ]),
                    new BlockStatementNode([
                        new LabelNode("if_0_else"),
                        new ReturnStatementNode(
                            new UnaryExpressionNode(
                                UnaryExpressionKind.UnaryMinus,
                                new MemberAccessExpressionNode("a")
                            )
                        ),
                        new GoToNode("if_0_end"),
                    ]),
                    new LabelNode("if_0_end"),
                ])
            )
        ]);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void RewriteIfWithoutElseStatementTest()
    {
        var tree = Parse(
            """
            function test(a: i32): i32 {
                if (a >= 0) {
                    return a;
                }

                return -a;
            }
            """);
        var expected = new SyntaxTree([
            new FunctionDeclarationNode(
                "test",
                [new ParameterNode("a", new TypeNode("i32"))],
                new TypeNode("i32"),
                new BlockStatementNode([
                    new IfStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.GreaterThanOrEqual,
                            new MemberAccessExpressionNode("a"),
                            LiteralExpressionNode.Number(0)
                        ),
                        new BlockStatementNode([
                            new GoToNode("if_0_then")
                        ]),
                        new BlockStatementNode([
                            new GoToNode("if_0_end")
                        ])
                    ),
                    new BlockStatementNode([
                        new LabelNode("if_0_then"),
                        new ReturnStatementNode(
                            new MemberAccessExpressionNode("a")
                        ),
                        new GoToNode("if_0_end"),
                    ]),
                    new LabelNode("if_0_end"),
                    new ReturnStatementNode(
                        new UnaryExpressionNode(
                            UnaryExpressionKind.UnaryMinus,
                            new MemberAccessExpressionNode("a")
                        )
                    ),
                ])
            )
        ]);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        Assert.That(tree, Is.EqualTo(expected));
    }
}