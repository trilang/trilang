using Trilang.Lower;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Lower;

public class ReplaceWhileLoopTests
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
    public void ReplaceWhileLoopTest()
    {
        var tree = Parse(
            """
            function test(a: i32): i32 {
                a = 0;

                while (a < 10) {
                    a += 1;
                }

                return a;
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
                    new ExpressionStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.Assignment,
                            new MemberAccessExpressionNode("a")
                            {
                                Reference = parameterMetadata,
                                AccessKind = PropertyAccessKind.Write,
                            },
                            new LiteralExpressionNode(LiteralExpressionKind.Number, 0)
                            {
                                ReturnTypeMetadata = TypeMetadata.I32
                            }
                        )
                        {
                            ReturnTypeMetadata = TypeMetadata.I32
                        }
                    ),
                    new BlockStatementNode([
                        new GoToNode("loop_0_start"),
                        new LabelNode("loop_0_start"),
                        new IfStatementNode(
                            new BinaryExpressionNode(
                                BinaryExpressionKind.LessThan,
                                new MemberAccessExpressionNode("a")
                                {
                                    Reference = parameterMetadata,
                                    AccessKind = PropertyAccessKind.Read,
                                },
                                new LiteralExpressionNode(LiteralExpressionKind.Number, 10)
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
                                new GoToNode("loop_0_end")
                            ])
                        ),
                        new BlockStatementNode([
                            new LabelNode("if_0_then"),
                            new ExpressionStatementNode(
                                new BinaryExpressionNode(
                                    BinaryExpressionKind.Assignment,
                                    new MemberAccessExpressionNode("a")
                                    {
                                        Reference = parameterMetadata,
                                        AccessKind = PropertyAccessKind.Write,
                                    },
                                    new BinaryExpressionNode(
                                        BinaryExpressionKind.Addition,
                                        new MemberAccessExpressionNode("a")
                                        {
                                            Reference = parameterMetadata,
                                            AccessKind = PropertyAccessKind.Read,
                                        },
                                        new LiteralExpressionNode(LiteralExpressionKind.Number, 1)
                                        {
                                            ReturnTypeMetadata = TypeMetadata.I32
                                        }
                                    )
                                    {
                                        ReturnTypeMetadata = TypeMetadata.I32
                                    }
                                )
                                {
                                    ReturnTypeMetadata = TypeMetadata.I32
                                }
                            ),
                            new GoToNode("loop_0_start"),
                        ]),
                        new LabelNode("loop_0_end"),
                    ]),
                    new ReturnStatementNode(
                        new MemberAccessExpressionNode("a")
                        {
                            Reference = parameterMetadata,
                            AccessKind = PropertyAccessKind.Read,
                        }
                    )
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
    public void ReplaceNestedWhileLoopTest()
    {
        var tree = Parse(
            """
            function test(a: i32): i32 {
                a = 0;

                while (a < 10) {
                    while (true) { }

                    a += 1;
                }

                return a;
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
                    new ExpressionStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.Assignment,
                            new MemberAccessExpressionNode("a")
                            {
                                Reference = parameterMetadata,
                                AccessKind = PropertyAccessKind.Write,
                            },
                            new LiteralExpressionNode(LiteralExpressionKind.Number, 0)
                            {
                                ReturnTypeMetadata = TypeMetadata.I32
                            }
                        )
                        {
                            ReturnTypeMetadata = TypeMetadata.I32
                        }
                    ),
                    new BlockStatementNode([
                        new GoToNode("loop_0_start"),
                        new LabelNode("loop_0_start"),
                        new IfStatementNode(
                            new BinaryExpressionNode(
                                BinaryExpressionKind.LessThan,
                                new MemberAccessExpressionNode("a")
                                {
                                    Reference = parameterMetadata,
                                    AccessKind = PropertyAccessKind.Read,
                                },
                                new LiteralExpressionNode(LiteralExpressionKind.Number, 10)
                                {
                                    ReturnTypeMetadata = TypeMetadata.I32
                                }
                            )
                            {
                                ReturnTypeMetadata = TypeMetadata.Bool
                            },
                            new BlockStatementNode([
                                new GoToNode("if_0_then"),
                            ]),
                            new BlockStatementNode([
                                new GoToNode("loop_0_end")
                            ])
                        ),
                        new BlockStatementNode([
                            new LabelNode("if_0_then"),
                            new BlockStatementNode([
                                new GoToNode("loop_1_start"),
                                new LabelNode("loop_1_start"),
                                new IfStatementNode(
                                    new LiteralExpressionNode(LiteralExpressionKind.Boolean, true)
                                    {
                                        ReturnTypeMetadata = TypeMetadata.Bool
                                    },
                                    new BlockStatementNode([
                                        new GoToNode("loop_1_start"),
                                    ]),
                                    new BlockStatementNode([
                                        new GoToNode("loop_1_end"),
                                    ])
                                ),
                                new LabelNode("loop_1_end"),
                            ]),
                            new ExpressionStatementNode(
                                new BinaryExpressionNode(
                                    BinaryExpressionKind.Assignment,
                                    new MemberAccessExpressionNode("a")
                                    {
                                        Reference = parameterMetadata,
                                        AccessKind = PropertyAccessKind.Write,
                                    },
                                    new BinaryExpressionNode(
                                        BinaryExpressionKind.Addition,
                                        new MemberAccessExpressionNode("a")
                                        {
                                            Reference = parameterMetadata,
                                            AccessKind = PropertyAccessKind.Read,
                                        },
                                        new LiteralExpressionNode(LiteralExpressionKind.Number, 1)
                                        {
                                            ReturnTypeMetadata = TypeMetadata.I32
                                        }
                                    )
                                    {
                                        ReturnTypeMetadata = TypeMetadata.I32
                                    }
                                )
                                {
                                    ReturnTypeMetadata = TypeMetadata.I32
                                }
                            ),
                            new GoToNode("loop_0_start"),
                        ]),
                        new LabelNode("loop_0_end"),
                    ]),
                    new ReturnStatementNode(
                        new MemberAccessExpressionNode("a")
                        {
                            Reference = parameterMetadata,
                            AccessKind = PropertyAccessKind.Read,
                        }
                    )
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
    public void ReplaceBreakInWhileLoopTest()
    {
        var tree = Parse(
            """
            function test(a: i32): i32 {
                a = 0;

                while (a < 10) {
                    a += 1;

                    if (a == 5) {
                        break;
                    }
                }

                return a;
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
                    new ExpressionStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.Assignment,
                            new MemberAccessExpressionNode("a")
                            {
                                Reference = parameterMetadata,
                                AccessKind = PropertyAccessKind.Write,
                            },
                            new LiteralExpressionNode(LiteralExpressionKind.Number, 0)
                            {
                                ReturnTypeMetadata = TypeMetadata.I32
                            }
                        )
                        {
                            ReturnTypeMetadata = TypeMetadata.I32
                        }
                    ),
                    new BlockStatementNode([
                        new GoToNode("loop_0_start"),
                        new LabelNode("loop_0_start"),
                        new IfStatementNode(
                            new BinaryExpressionNode(
                                BinaryExpressionKind.LessThan,
                                new MemberAccessExpressionNode("a")
                                {
                                    Reference = parameterMetadata,
                                    AccessKind = PropertyAccessKind.Read,
                                },
                                new LiteralExpressionNode(LiteralExpressionKind.Number, 10)
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
                                new GoToNode("loop_0_end")
                            ])
                        ),
                        new BlockStatementNode([
                            new LabelNode("if_0_then"),
                            new ExpressionStatementNode(
                                new BinaryExpressionNode(
                                    BinaryExpressionKind.Assignment,
                                    new MemberAccessExpressionNode("a")
                                    {
                                        Reference = parameterMetadata,
                                        AccessKind = PropertyAccessKind.Write,
                                    },
                                    new BinaryExpressionNode(
                                        BinaryExpressionKind.Addition,
                                        new MemberAccessExpressionNode("a")
                                        {
                                            Reference = parameterMetadata,
                                            AccessKind = PropertyAccessKind.Read,
                                        },
                                        new LiteralExpressionNode(LiteralExpressionKind.Number, 1)
                                        {
                                            ReturnTypeMetadata = TypeMetadata.I32
                                        }
                                    )
                                    {
                                        ReturnTypeMetadata = TypeMetadata.I32
                                    }
                                )
                                {
                                    ReturnTypeMetadata = TypeMetadata.I32
                                }
                            ),
                            new IfStatementNode(
                                new BinaryExpressionNode(
                                    BinaryExpressionKind.Equality,
                                    new MemberAccessExpressionNode("a")
                                    {
                                        Reference = parameterMetadata,
                                        AccessKind = PropertyAccessKind.Read,
                                    },
                                    new LiteralExpressionNode(LiteralExpressionKind.Number, 5)
                                    {
                                        ReturnTypeMetadata = TypeMetadata.I32
                                    }
                                )
                                {
                                    ReturnTypeMetadata = TypeMetadata.Bool
                                },
                                new BlockStatementNode([
                                    new GoToNode("loop_0_end"),
                                ]),
                                new BlockStatementNode([
                                    new GoToNode("if_1_end"),
                                ])
                            ),
                            new LabelNode("if_1_end"),
                            new GoToNode("loop_0_start"),
                        ]),
                        new LabelNode("loop_0_end"),
                    ]),
                    new ReturnStatementNode(
                        new MemberAccessExpressionNode("a")
                        {
                            Reference = parameterMetadata,
                            AccessKind = PropertyAccessKind.Read,
                        }
                    )
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
    public void ReplaceContinueInWhileLoopTest()
    {
        var tree = Parse(
            """
            function test(a: i32): i32 {
                a = 0;

                while (a < 10) {
                    a += 1;

                    if (a == 5) {
                        continue;
                    }
                }

                return a;
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
                    new ExpressionStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.Assignment,
                            new MemberAccessExpressionNode("a")
                            {
                                Reference = parameterMetadata,
                                AccessKind = PropertyAccessKind.Write,
                            },
                            new LiteralExpressionNode(LiteralExpressionKind.Number, 0)
                            {
                                ReturnTypeMetadata = TypeMetadata.I32
                            }
                        )
                        {
                            ReturnTypeMetadata = TypeMetadata.I32
                        }
                    ),
                    new BlockStatementNode([
                        new GoToNode("loop_0_start"),
                        new LabelNode("loop_0_start"),
                        new IfStatementNode(
                            new BinaryExpressionNode(
                                BinaryExpressionKind.LessThan,
                                new MemberAccessExpressionNode("a")
                                {
                                    Reference = parameterMetadata,
                                    AccessKind = PropertyAccessKind.Read,
                                },
                                new LiteralExpressionNode(LiteralExpressionKind.Number, 10)
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
                                new GoToNode("loop_0_end")
                            ])
                        ),
                        new BlockStatementNode([
                            new LabelNode("if_0_then"),
                            new ExpressionStatementNode(
                                new BinaryExpressionNode(
                                    BinaryExpressionKind.Assignment,
                                    new MemberAccessExpressionNode("a")
                                    {
                                        Reference = parameterMetadata,
                                        AccessKind = PropertyAccessKind.Write,
                                    },
                                    new BinaryExpressionNode(
                                        BinaryExpressionKind.Addition,
                                        new MemberAccessExpressionNode("a")
                                        {
                                            Reference = parameterMetadata,
                                            AccessKind = PropertyAccessKind.Read,
                                        },
                                        new LiteralExpressionNode(LiteralExpressionKind.Number, 1)
                                        {
                                            ReturnTypeMetadata = TypeMetadata.I32
                                        }
                                    )
                                    {
                                        ReturnTypeMetadata = TypeMetadata.I32
                                    }
                                )
                                {
                                    ReturnTypeMetadata = TypeMetadata.I32
                                }
                            ),
                            new IfStatementNode(
                                new BinaryExpressionNode(
                                    BinaryExpressionKind.Equality,
                                    new MemberAccessExpressionNode("a")
                                    {
                                        Reference = parameterMetadata,
                                        AccessKind = PropertyAccessKind.Read,
                                    },
                                    new LiteralExpressionNode(LiteralExpressionKind.Number, 5)
                                    {
                                        ReturnTypeMetadata = TypeMetadata.I32
                                    }
                                )
                                {
                                    ReturnTypeMetadata = TypeMetadata.Bool
                                },
                                new BlockStatementNode([
                                    new GoToNode("loop_0_start"),
                                ]),
                                new BlockStatementNode([
                                    new GoToNode("if_1_end"),
                                ])
                            ),
                            new LabelNode("if_1_end"),
                            new GoToNode("loop_0_start"),
                        ]),
                        new LabelNode("loop_0_end"),
                    ]),
                    new ReturnStatementNode(
                        new MemberAccessExpressionNode("a")
                        {
                            Reference = parameterMetadata,
                            AccessKind = PropertyAccessKind.Read,
                        }
                    )
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