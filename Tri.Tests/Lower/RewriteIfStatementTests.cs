using Trilang.Lower;
using Trilang.Metadata;
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
        var parameterMetadata = new ParameterMetadata("a", TypeMetadata.I32);
        var expected = new SyntaxTree([
            new FunctionDeclarationNode(
                "test",
                [
                    new ParameterNode("a", new TypeNode("i32") { Metadata = TypeMetadata.I32 })
                    {
                        Metadata = parameterMetadata,
                    }
                ],
                new TypeNode("i32") { Metadata = TypeMetadata.I32 },
                new BlockStatementNode([
                    new IfStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.GreaterThanOrEqual,
                            new MemberAccessExpressionNode("a")
                            {
                                Reference = parameterMetadata,
                                AccessKind = PropertyAccessKind.Read,
                            },
                            new LiteralExpressionNode(LiteralExpressionKind.Number, 0)
                            {
                                ReturnTypeMetadata = TypeMetadata.I32
                            }
                        )
                        {
                            ReturnTypeMetadata = TypeMetadata.Bool
                        },
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
                            {
                                Reference = parameterMetadata,
                                AccessKind = PropertyAccessKind.Read,
                            }
                        ),
                        new GoToNode("if_0_end"),
                    ]),
                    new BlockStatementNode([
                        new LabelNode("if_0_else"),
                        new ReturnStatementNode(
                            new UnaryExpressionNode(
                                UnaryExpressionKind.UnaryMinus,
                                new MemberAccessExpressionNode("a")
                                {
                                    Reference = parameterMetadata,
                                    AccessKind = PropertyAccessKind.Read,
                                }
                            )
                            {
                                ReturnTypeMetadata = TypeMetadata.I32
                            }
                        ),
                        new GoToNode("if_0_end"),
                    ]),
                    new LabelNode("if_0_end"),
                ])
            )
            {
                Metadata = new FunctionMetadata(
                    "test",
                    [parameterMetadata],
                    new FunctionTypeMetadata([TypeMetadata.I32], TypeMetadata.I32)
                )
            }
        ]);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
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
        var parameterMetadata = new ParameterMetadata("a", TypeMetadata.I32);
        var expected = new SyntaxTree([
            new FunctionDeclarationNode(
                "test",
                [
                    new ParameterNode("a", new TypeNode("i32") { Metadata = TypeMetadata.I32 })
                    {
                        Metadata = parameterMetadata,
                    }
                ],
                new TypeNode("i32") { Metadata = TypeMetadata.I32 },
                new BlockStatementNode([
                    new IfStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.GreaterThanOrEqual,
                            new MemberAccessExpressionNode("a")
                            {
                                Reference = parameterMetadata,
                                AccessKind = PropertyAccessKind.Read,
                            },
                            new LiteralExpressionNode(LiteralExpressionKind.Number, 0)
                            {
                                ReturnTypeMetadata = TypeMetadata.I32
                            }
                        )
                        {
                            ReturnTypeMetadata = TypeMetadata.Bool
                        },
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
                            {
                                Reference = parameterMetadata,
                                AccessKind = PropertyAccessKind.Read,
                            }
                        ),
                        new GoToNode("if_0_end"),
                    ]),
                    new LabelNode("if_0_end"),
                    new ReturnStatementNode(
                        new UnaryExpressionNode(
                            UnaryExpressionKind.UnaryMinus,
                            new MemberAccessExpressionNode("a")
                            {
                                Reference = parameterMetadata,
                                AccessKind = PropertyAccessKind.Read,
                            }
                        )
                        {
                            ReturnTypeMetadata = TypeMetadata.I32
                        }
                    ),
                ])
            )
            {
                Metadata = new FunctionMetadata(
                    "test",
                    [parameterMetadata],
                    new FunctionTypeMetadata([TypeMetadata.I32], TypeMetadata.I32)
                )
            }
        ]);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }
}