using Trilang.Lower;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using Type = Trilang.Semantics.Model.Type;

namespace Tri.Tests.Lower;

public class ReplaceConditionalOperatorsTests
{
    private static SemanticTree Parse(string code)
    {
        var parser = new Parser();
        var tree = parser.Parse(code);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        return semanticTree;
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
        var expected = new SemanticTree([
            new FunctionDeclaration(
                "test",
                [
                    new Parameter("a", new Type("bool") { Metadata = TypeMetadata.Bool })
                    {
                        Metadata = aParameter,
                    },
                    new Parameter("b", new Type("bool") { Metadata = TypeMetadata.Bool })
                    {
                        Metadata = bParameter,
                    },
                ],
                new Type("i32") { Metadata = TypeMetadata.I32 },
                new BlockStatement([
                    new IfStatement(
                        new ExpressionBlock([
                            new VariableDeclaration(
                                "cond_0",
                                new Type("bool") { Metadata = TypeMetadata.Bool },
                                new MemberAccessExpression("a")
                                {
                                    AccessKind = MemberAccessKind.Read,
                                    Reference = aParameter,
                                }
                            )
                            {
                                Metadata = new VariableMetadata("cond_0", TypeMetadata.Bool),
                            },
                            new IfStatement(
                                new MemberAccessExpression("cond_0")
                                {
                                    AccessKind = MemberAccessKind.Read,
                                    Reference = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                },
                                new BlockStatement([
                                    new GoTo("if_1_then"),
                                ]),
                                new BlockStatement([
                                    new GoTo("if_1_end"),
                                ])
                            ),
                            new BlockStatement([
                                new Label("if_1_then"),
                                new ExpressionStatement(
                                    new BinaryExpression(
                                        BinaryExpressionKind.Assignment,
                                        new MemberAccessExpression("cond_0")
                                        {
                                            AccessKind = MemberAccessKind.Write,
                                            Reference = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                        },
                                        new MemberAccessExpression("b")
                                        {
                                            AccessKind = MemberAccessKind.Read,
                                            Reference = bParameter,
                                        }
                                    )
                                ),
                                new GoTo("if_1_end"),
                            ]),
                            new Label("if_1_end"),
                            new ExpressionStatement(
                                new MemberAccessExpression("cond_0")
                                {
                                    AccessKind = MemberAccessKind.Read,
                                    Reference = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                }
                            )
                        ]),
                        new BlockStatement([
                            new GoTo("if_0_then"),
                        ]),
                        new BlockStatement([
                            new GoTo("if_0_end"),
                        ])
                    ),
                    new BlockStatement([
                        new Label("if_0_then"),
                        new ReturnStatement(
                            new LiteralExpression(LiteralExpressionKind.Integer, 1)
                            {
                                ReturnTypeMetadata = TypeMetadata.I32,
                            }
                        ),
                        new GoTo("if_0_end"),
                    ]),
                    new Label("if_0_end"),
                    new ReturnStatement(
                        new LiteralExpression(LiteralExpressionKind.Integer, 0)
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

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
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
        var expected = new SemanticTree([
            new FunctionDeclaration(
                "test",
                [
                    new Parameter("a", new Type("bool") { Metadata = TypeMetadata.Bool })
                    {
                        Metadata = aParameter,
                    },
                    new Parameter("b", new Type("bool") { Metadata = TypeMetadata.Bool })
                    {
                        Metadata = bParameter,
                    },
                ],
                new Type("i32") { Metadata = TypeMetadata.I32 },
                new BlockStatement([
                    new IfStatement(
                        new ExpressionBlock([
                            new VariableDeclaration(
                                "cond_0",
                                new Type("bool") { Metadata = TypeMetadata.Bool },
                                new MemberAccessExpression("a")
                                {
                                    AccessKind = MemberAccessKind.Read,
                                    Reference = aParameter,
                                }
                            )
                            {
                                Metadata = new VariableMetadata("cond_0", TypeMetadata.Bool),
                            },
                            new IfStatement(
                                new UnaryExpression(
                                    UnaryExpressionKind.LogicalNot,
                                    new MemberAccessExpression("cond_0")
                                    {
                                        AccessKind = MemberAccessKind.Read,
                                        Reference = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                    }
                                )
                                {
                                    ReturnTypeMetadata = TypeMetadata.Bool,
                                },
                                new BlockStatement([
                                    new GoTo("if_1_then"),
                                ]),
                                new BlockStatement([
                                    new GoTo("if_1_end"),
                                ])
                            ),
                            new BlockStatement([
                                new Label("if_1_then"),
                                new ExpressionStatement(
                                    new BinaryExpression(
                                        BinaryExpressionKind.Assignment,
                                        new MemberAccessExpression("cond_0")
                                        {
                                            AccessKind = MemberAccessKind.Write,
                                            Reference = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                        },
                                        new MemberAccessExpression("b")
                                        {
                                            AccessKind = MemberAccessKind.Read,
                                            Reference = bParameter,
                                        }
                                    )
                                ),
                                new GoTo("if_1_end"),
                            ]),
                            new Label("if_1_end"),
                            new ExpressionStatement(
                                new MemberAccessExpression("cond_0")
                                {
                                    AccessKind = MemberAccessKind.Read,
                                    Reference = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                }
                            )
                        ]),
                        new BlockStatement([
                            new GoTo("if_0_then"),
                        ]),
                        new BlockStatement([
                            new GoTo("if_0_end"),
                        ])
                    ),
                    new BlockStatement([
                        new Label("if_0_then"),
                        new ReturnStatement(
                            new LiteralExpression(LiteralExpressionKind.Integer, 1)
                            {
                                ReturnTypeMetadata = TypeMetadata.I32,
                            }
                        ),
                        new GoTo("if_0_end"),
                    ]),
                    new Label("if_0_end"),
                    new ReturnStatement(
                        new LiteralExpression(LiteralExpressionKind.Integer, 0)
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

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
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
        var expected = new SemanticTree([
            new FunctionDeclaration(
                "test",
                [
                    new Parameter("a", new Type("bool") { Metadata = TypeMetadata.Bool })
                    {
                        Metadata = aParameter,
                    },
                    new Parameter("b", new Type("bool") { Metadata = TypeMetadata.Bool })
                    {
                        Metadata = bParameter,
                    },
                    new Parameter("c", new Type("bool") { Metadata = TypeMetadata.Bool })
                    {
                        Metadata = cParameter,
                    }
                ],
                new Type("i32") { Metadata = TypeMetadata.I32 },
                new BlockStatement([
                    new IfStatement(
                        new ExpressionBlock([
                            new VariableDeclaration(
                                "cond_1",
                                new Type("bool") { Metadata = TypeMetadata.Bool },
                                new ExpressionBlock([
                                    new VariableDeclaration(
                                        "cond_0",
                                        new Type("bool") { Metadata = TypeMetadata.Bool },
                                        new MemberAccessExpression("a")
                                        {
                                            AccessKind = MemberAccessKind.Read,
                                            Reference = aParameter,
                                        }
                                    )
                                    {
                                        Metadata = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                    },
                                    new IfStatement(
                                        new MemberAccessExpression("cond_0")
                                        {
                                            AccessKind = MemberAccessKind.Read,
                                            Reference = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                        },
                                        new BlockStatement([
                                            new GoTo("if_1_then"),
                                        ]),
                                        new BlockStatement([
                                            new GoTo("if_1_end"),
                                        ])
                                    ),
                                    new BlockStatement([
                                        new Label("if_1_then"),
                                        new ExpressionStatement(
                                            new BinaryExpression(
                                                BinaryExpressionKind.Assignment,
                                                new MemberAccessExpression("cond_0")
                                                {
                                                    AccessKind = MemberAccessKind.Write,
                                                    Reference = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                                },
                                                new MemberAccessExpression("b")
                                                {
                                                    AccessKind = MemberAccessKind.Read,
                                                    Reference = bParameter,
                                                }
                                            )
                                        ),
                                        new GoTo("if_1_end"),
                                    ]),
                                    new Label("if_1_end"),
                                    new ExpressionStatement(
                                        new MemberAccessExpression("cond_0")
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
                            new IfStatement(
                                new MemberAccessExpression("cond_1")
                                {
                                    AccessKind = MemberAccessKind.Read,
                                    Reference = new VariableMetadata("cond_1", TypeMetadata.Bool),
                                },
                                new BlockStatement([
                                    new GoTo("if_2_then"),
                                ]),
                                new BlockStatement([
                                    new GoTo("if_2_end"),
                                ])
                            ),
                            new BlockStatement([
                                new Label("if_2_then"),
                                new ExpressionStatement(
                                    new BinaryExpression(
                                        BinaryExpressionKind.Assignment,
                                        new MemberAccessExpression("cond_1")
                                        {
                                            AccessKind = MemberAccessKind.Write,
                                            Reference = new VariableMetadata("cond_1", TypeMetadata.Bool),
                                        },
                                        new MemberAccessExpression("c")
                                        {
                                            AccessKind = MemberAccessKind.Read,
                                            Reference = cParameter,
                                        }
                                    )
                                ),
                                new GoTo("if_2_end"),
                            ]),
                            new Label("if_2_end"),
                            new ExpressionStatement(
                                new MemberAccessExpression("cond_1")
                                {
                                    AccessKind = MemberAccessKind.Read,
                                    Reference = new VariableMetadata("cond_1", TypeMetadata.Bool),
                                }
                            )
                        ]),
                        new BlockStatement([
                            new GoTo("if_0_then"),
                        ]),
                        new BlockStatement([
                            new GoTo("if_0_end"),
                        ])
                    ),
                    new BlockStatement([
                        new Label("if_0_then"),
                        new ReturnStatement(
                            new LiteralExpression(LiteralExpressionKind.Integer, 1)
                            {
                                ReturnTypeMetadata = TypeMetadata.I32,
                            }
                        ),
                        new GoTo("if_0_end"),
                    ]),
                    new Label("if_0_end"),
                    new ReturnStatement(
                        new LiteralExpression(LiteralExpressionKind.Integer, 0)
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

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }
}