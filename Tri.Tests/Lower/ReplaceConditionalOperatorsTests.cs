using Trilang.Lower;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Lower;

public class ReplaceConditionalOperatorsTests
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
    public void ReplaceConditionalAndTest()
    {
        var tree = Parse(
            """
            function test(a: bool, b: bool): i32 {
                if (a && b) {
                    return 1;
                }

                return 0;
            }
            """);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        var aParameter = new ParameterMetadata("a", TypeMetadata.Bool);
        var bParameter = new ParameterMetadata("b", TypeMetadata.Bool);
        var expected = new SyntaxTree([
            new FunctionDeclarationNode(
                "test",
                [
                    new ParameterNode("a", new TypeNode("bool") { Metadata = TypeMetadata.Bool })
                    {
                        Metadata = aParameter,
                    },
                    new ParameterNode("b", new TypeNode("bool") { Metadata = TypeMetadata.Bool })
                    {
                        Metadata = bParameter,
                    },
                ],
                new TypeNode("i32") { Metadata = TypeMetadata.I32 },
                new BlockStatementNode([
                    new IfStatementNode(
                        new ExpressionBlockNode([
                            new VariableDeclarationStatementNode(
                                "cond_0",
                                new TypeNode("bool") { Metadata = TypeMetadata.Bool },
                                new MemberAccessExpressionNode("a")
                                {
                                    AccessKind = MemberAccessKind.Read,
                                    Reference = aParameter,
                                }
                            )
                            {
                                Metadata = new VariableMetadata("cond_0", TypeMetadata.Bool),
                            },
                            new IfStatementNode(
                                new MemberAccessExpressionNode("cond_0")
                                {
                                    AccessKind = MemberAccessKind.Read,
                                    Reference = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                },
                                new BlockStatementNode([
                                    new GoToNode("if_1_then"),
                                ]),
                                new BlockStatementNode([
                                    new GoToNode("if_1_end"),
                                ])
                            ),
                            new BlockStatementNode([
                                new LabelNode("if_1_then"),
                                new ExpressionStatementNode(
                                    new BinaryExpressionNode(
                                        BinaryExpressionKind.Assignment,
                                        new MemberAccessExpressionNode("cond_0")
                                        {
                                            AccessKind = MemberAccessKind.Write,
                                            Reference = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                        },
                                        new MemberAccessExpressionNode("b")
                                        {
                                            AccessKind = MemberAccessKind.Read,
                                            Reference = bParameter,
                                        }
                                    )
                                ),
                                new GoToNode("if_1_end"),
                            ]),
                            new LabelNode("if_1_end"),
                            new ExpressionStatementNode(
                                new MemberAccessExpressionNode("cond_0")
                                {
                                    AccessKind = MemberAccessKind.Read,
                                    Reference = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                }
                            )
                        ]),
                        new BlockStatementNode([
                            new GoToNode("if_0_then"),
                        ]),
                        new BlockStatementNode([
                            new GoToNode("if_0_end"),
                        ])
                    ),
                    new BlockStatementNode([
                        new LabelNode("if_0_then"),
                        new ReturnStatementNode(
                            new LiteralExpressionNode(LiteralExpressionKind.Integer, 1)
                            {
                                ReturnTypeMetadata = TypeMetadata.I32,
                            }
                        ),
                        new GoToNode("if_0_end"),
                    ]),
                    new LabelNode("if_0_end"),
                    new ReturnStatementNode(
                        new LiteralExpressionNode(LiteralExpressionKind.Integer, 0)
                        {
                            ReturnTypeMetadata = TypeMetadata.I32,
                        }
                    ),
                ])
            )
            {
                Metadata = new FunctionMetadata(
                    "test",
                    [aParameter, bParameter],
                    new FunctionTypeMetadata([TypeMetadata.Bool, TypeMetadata.Bool], TypeMetadata.I32)
                ),
            }
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ReplaceConditionalOrTest()
    {
        var tree = Parse(
            """
            function test(a: bool, b: bool): i32 {
                if (a || b) {
                    return 1;
                }

                return 0;
            }
            """);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        var aParameter = new ParameterMetadata("a", TypeMetadata.Bool);
        var bParameter = new ParameterMetadata("b", TypeMetadata.Bool);
        var expected = new SyntaxTree([
            new FunctionDeclarationNode(
                "test",
                [
                    new ParameterNode("a", new TypeNode("bool") { Metadata = TypeMetadata.Bool })
                    {
                        Metadata = aParameter,
                    },
                    new ParameterNode("b", new TypeNode("bool") { Metadata = TypeMetadata.Bool })
                    {
                        Metadata = bParameter,
                    },
                ],
                new TypeNode("i32") { Metadata = TypeMetadata.I32 },
                new BlockStatementNode([
                    new IfStatementNode(
                        new ExpressionBlockNode([
                            new VariableDeclarationStatementNode(
                                "cond_0",
                                new TypeNode("bool") { Metadata = TypeMetadata.Bool },
                                new MemberAccessExpressionNode("a")
                                {
                                    AccessKind = MemberAccessKind.Read,
                                    Reference = aParameter,
                                }
                            )
                            {
                                Metadata = new VariableMetadata("cond_0", TypeMetadata.Bool),
                            },
                            new IfStatementNode(
                                new UnaryExpressionNode(
                                    UnaryExpressionKind.LogicalNot,
                                    new MemberAccessExpressionNode("cond_0")
                                    {
                                        AccessKind = MemberAccessKind.Read,
                                        Reference = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                    }
                                )
                                {
                                    ReturnTypeMetadata = TypeMetadata.Bool,
                                },
                                new BlockStatementNode([
                                    new GoToNode("if_1_then"),
                                ]),
                                new BlockStatementNode([
                                    new GoToNode("if_1_end"),
                                ])
                            ),
                            new BlockStatementNode([
                                new LabelNode("if_1_then"),
                                new ExpressionStatementNode(
                                    new BinaryExpressionNode(
                                        BinaryExpressionKind.Assignment,
                                        new MemberAccessExpressionNode("cond_0")
                                        {
                                            AccessKind = MemberAccessKind.Write,
                                            Reference = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                        },
                                        new MemberAccessExpressionNode("b")
                                        {
                                            AccessKind = MemberAccessKind.Read,
                                            Reference = bParameter,
                                        }
                                    )
                                ),
                                new GoToNode("if_1_end"),
                            ]),
                            new LabelNode("if_1_end"),
                            new ExpressionStatementNode(
                                new MemberAccessExpressionNode("cond_0")
                                {
                                    AccessKind = MemberAccessKind.Read,
                                    Reference = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                }
                            )
                        ]),
                        new BlockStatementNode([
                            new GoToNode("if_0_then"),
                        ]),
                        new BlockStatementNode([
                            new GoToNode("if_0_end"),
                        ])
                    ),
                    new BlockStatementNode([
                        new LabelNode("if_0_then"),
                        new ReturnStatementNode(
                            new LiteralExpressionNode(LiteralExpressionKind.Integer, 1)
                            {
                                ReturnTypeMetadata = TypeMetadata.I32,
                            }
                        ),
                        new GoToNode("if_0_end"),
                    ]),
                    new LabelNode("if_0_end"),
                    new ReturnStatementNode(
                        new LiteralExpressionNode(LiteralExpressionKind.Integer, 0)
                        {
                            ReturnTypeMetadata = TypeMetadata.I32,
                        }
                    ),
                ])
            )
            {
                Metadata = new FunctionMetadata(
                    "test",
                    [aParameter, bParameter],
                    new FunctionTypeMetadata([TypeMetadata.Bool, TypeMetadata.Bool], TypeMetadata.I32)
                ),
            }
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ReplaceNestedConditionalAndTest()
    {
        var tree = Parse(
            """
            function test(a: bool, b: bool, c: bool): i32 {
                if (a && b && c) {
                    return 1;
                }

                return 0;
            }
            """);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        var aParameter = new ParameterMetadata("a", TypeMetadata.Bool);
        var bParameter = new ParameterMetadata("b", TypeMetadata.Bool);
        var cParameter = new ParameterMetadata("c", TypeMetadata.Bool);
        var expected = new SyntaxTree([
            new FunctionDeclarationNode(
                "test",
                [
                    new ParameterNode("a", new TypeNode("bool") { Metadata = TypeMetadata.Bool })
                    {
                        Metadata = aParameter,
                    },
                    new ParameterNode("b", new TypeNode("bool") { Metadata = TypeMetadata.Bool })
                    {
                        Metadata = bParameter,
                    },
                    new ParameterNode("c", new TypeNode("bool") { Metadata = TypeMetadata.Bool })
                    {
                        Metadata = cParameter,
                    }
                ],
                new TypeNode("i32") { Metadata = TypeMetadata.I32 },
                new BlockStatementNode([
                    new IfStatementNode(
                        new ExpressionBlockNode([
                            new VariableDeclarationStatementNode(
                                "cond_1",
                                new TypeNode("bool") { Metadata = TypeMetadata.Bool },
                                new ExpressionBlockNode([
                                    new VariableDeclarationStatementNode(
                                        "cond_0",
                                        new TypeNode("bool") { Metadata = TypeMetadata.Bool },
                                        new MemberAccessExpressionNode("a")
                                        {
                                            AccessKind = MemberAccessKind.Read,
                                            Reference = aParameter,
                                        }
                                    )
                                    {
                                        Metadata = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                    },
                                    new IfStatementNode(
                                        new MemberAccessExpressionNode("cond_0")
                                        {
                                            AccessKind = MemberAccessKind.Read,
                                            Reference = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                        },
                                        new BlockStatementNode([
                                            new GoToNode("if_1_then"),
                                        ]),
                                        new BlockStatementNode([
                                            new GoToNode("if_1_end"),
                                        ])
                                    ),
                                    new BlockStatementNode([
                                        new LabelNode("if_1_then"),
                                        new ExpressionStatementNode(
                                            new BinaryExpressionNode(
                                                BinaryExpressionKind.Assignment,
                                                new MemberAccessExpressionNode("cond_0")
                                                {
                                                    AccessKind = MemberAccessKind.Write,
                                                    Reference = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                                },
                                                new MemberAccessExpressionNode("b")
                                                {
                                                    AccessKind = MemberAccessKind.Read,
                                                    Reference = bParameter,
                                                }
                                            )
                                        ),
                                        new GoToNode("if_1_end"),
                                    ]),
                                    new LabelNode("if_1_end"),
                                    new ExpressionStatementNode(
                                        new MemberAccessExpressionNode("cond_0")
                                        {
                                            AccessKind = MemberAccessKind.Read,
                                            Reference = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                        }
                                    )
                                ])
                            )
                            {
                                Metadata = new VariableMetadata("cond_1", TypeMetadata.Bool),
                            },
                            new IfStatementNode(
                                new MemberAccessExpressionNode("cond_1")
                                {
                                    AccessKind = MemberAccessKind.Read,
                                    Reference = new VariableMetadata("cond_1", TypeMetadata.Bool),
                                },
                                new BlockStatementNode([
                                    new GoToNode("if_2_then"),
                                ]),
                                new BlockStatementNode([
                                    new GoToNode("if_2_end"),
                                ])
                            ),
                            new BlockStatementNode([
                                new LabelNode("if_2_then"),
                                new ExpressionStatementNode(
                                    new BinaryExpressionNode(
                                        BinaryExpressionKind.Assignment,
                                        new MemberAccessExpressionNode("cond_1")
                                        {
                                            AccessKind = MemberAccessKind.Write,
                                            Reference = new VariableMetadata("cond_1", TypeMetadata.Bool),
                                        },
                                        new MemberAccessExpressionNode("c")
                                        {
                                            AccessKind = MemberAccessKind.Read,
                                            Reference = cParameter,
                                        }
                                    )
                                ),
                                new GoToNode("if_2_end"),
                            ]),
                            new LabelNode("if_2_end"),
                            new ExpressionStatementNode(
                                new MemberAccessExpressionNode("cond_1")
                                {
                                    AccessKind = MemberAccessKind.Read,
                                    Reference = new VariableMetadata("cond_1", TypeMetadata.Bool),
                                }
                            )
                        ]),
                        new BlockStatementNode([
                            new GoToNode("if_0_then"),
                        ]),
                        new BlockStatementNode([
                            new GoToNode("if_0_end"),
                        ])
                    ),
                    new BlockStatementNode([
                        new LabelNode("if_0_then"),
                        new ReturnStatementNode(
                            new LiteralExpressionNode(LiteralExpressionKind.Integer, 1)
                            {
                                ReturnTypeMetadata = TypeMetadata.I32,
                            }
                        ),
                        new GoToNode("if_0_end"),
                    ]),
                    new LabelNode("if_0_end"),
                    new ReturnStatementNode(
                        new LiteralExpressionNode(LiteralExpressionKind.Integer, 0)
                        {
                            ReturnTypeMetadata = TypeMetadata.I32,
                        }
                    ),
                ])
            )
            {
                Metadata = new FunctionMetadata(
                    "test",
                    [aParameter, bParameter, cParameter],
                    new FunctionTypeMetadata([TypeMetadata.Bool, TypeMetadata.Bool, TypeMetadata.Bool], TypeMetadata.I32)
                ),
            }
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }
}