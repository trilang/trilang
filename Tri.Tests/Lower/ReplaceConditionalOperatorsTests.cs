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
            public test(a: bool, b: bool): i32 {
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
        var expected = new SemanticTree(null, [
            new FunctionDeclaration(
                null,
                AccessModifier.Public,
                "test",
                [
                    new Parameter(null, "a", new Type(null, "bool") { Metadata = TypeMetadata.Bool })
                    {
                        Metadata = aParameter,
                    },
                    new Parameter(null, "b", new Type(null, "bool") { Metadata = TypeMetadata.Bool })
                    {
                        Metadata = bParameter,
                    },
                ],
                new Type(null, "i32") { Metadata = TypeMetadata.I32 },
                new BlockStatement(null, [
                    new IfStatement(
                        null,
                        new ExpressionBlock([
                            new VariableDeclaration(
                                null,
                                "cond_0",
                                new Type(null, "bool") { Metadata = TypeMetadata.Bool },
                                new MemberAccessExpression(null, "a")
                                {
                                    AccessKind = MemberAccessKind.Read,
                                    Reference = aParameter,
                                }
                            )
                            {
                                Metadata = new VariableMetadata("cond_0", TypeMetadata.Bool),
                            },
                            new IfStatement(
                                null,
                                new MemberAccessExpression(null, "cond_0")
                                {
                                    AccessKind = MemberAccessKind.Read,
                                    Reference = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                },
                                new BlockStatement(null, [
                                    new GoTo("if_1_then"),
                                ]),
                                new BlockStatement(null, [
                                    new GoTo("if_1_end"),
                                ])
                            ),
                            new BlockStatement(null, [
                                new Label("if_1_then"),
                                new ExpressionStatement(
                                    null,
                                    new BinaryExpression(
                                        null,
                                        BinaryExpressionKind.Assignment,
                                        new MemberAccessExpression(null, "cond_0")
                                        {
                                            AccessKind = MemberAccessKind.Write,
                                            Reference = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                        },
                                        new MemberAccessExpression(null, "b")
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
                                null,
                                new MemberAccessExpression(null, "cond_0")
                                {
                                    AccessKind = MemberAccessKind.Read,
                                    Reference = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                }
                            )
                        ]),
                        new BlockStatement(null, [
                            new GoTo("if_0_then"),
                        ]),
                        new BlockStatement(null, [
                            new GoTo("if_0_end"),
                        ])
                    ),
                    new BlockStatement(null, [
                        new Label("if_0_then"),
                        new ReturnStatement(
                            null,
                            new LiteralExpression(null, LiteralExpressionKind.Integer, 1)
                            {
                                ReturnTypeMetadata = TypeMetadata.I32,
                            }
                        ),
                        new GoTo("if_0_end"),
                    ]),
                    new Label("if_0_end"),
                    new ReturnStatement(
                        null,
                        new LiteralExpression(null, LiteralExpressionKind.Integer, 0)
                        {
                            ReturnTypeMetadata = TypeMetadata.I32,
                        }
                    ),
                ])
            )
            {
                Metadata = new FunctionMetadata(
                    AccessModifierMetadata.Public,
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
            public test(a: bool, b: bool): i32 {
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
        var expected = new SemanticTree(null, [
            new FunctionDeclaration(
                null,
                AccessModifier.Public,
                "test",
                [
                    new Parameter(null, "a", new Type(null, "bool") { Metadata = TypeMetadata.Bool })
                    {
                        Metadata = aParameter,
                    },
                    new Parameter(null, "b", new Type(null, "bool") { Metadata = TypeMetadata.Bool })
                    {
                        Metadata = bParameter,
                    },
                ],
                new Type(null, "i32") { Metadata = TypeMetadata.I32 },
                new BlockStatement(null, [
                    new IfStatement(
                        null,
                        new ExpressionBlock([
                            new VariableDeclaration(
                                null,
                                "cond_0",
                                new Type(null, "bool") { Metadata = TypeMetadata.Bool },
                                new MemberAccessExpression(null, "a")
                                {
                                    AccessKind = MemberAccessKind.Read,
                                    Reference = aParameter,
                                }
                            )
                            {
                                Metadata = new VariableMetadata("cond_0", TypeMetadata.Bool),
                            },
                            new IfStatement(
                                null,
                                new UnaryExpression(
                                    null,
                                    UnaryExpressionKind.LogicalNot,
                                    new MemberAccessExpression(null, "cond_0")
                                    {
                                        AccessKind = MemberAccessKind.Read,
                                        Reference = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                    }
                                )
                                {
                                    ReturnTypeMetadata = TypeMetadata.Bool,
                                },
                                new BlockStatement(null, [
                                    new GoTo("if_1_then"),
                                ]),
                                new BlockStatement(null, [
                                    new GoTo("if_1_end"),
                                ])
                            ),
                            new BlockStatement(null, [
                                new Label("if_1_then"),
                                new ExpressionStatement(
                                    null,
                                    new BinaryExpression(
                                        null,
                                        BinaryExpressionKind.Assignment,
                                        new MemberAccessExpression(null, "cond_0")
                                        {
                                            AccessKind = MemberAccessKind.Write,
                                            Reference = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                        },
                                        new MemberAccessExpression(null, "b")
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
                                null,
                                new MemberAccessExpression(null, "cond_0")
                                {
                                    AccessKind = MemberAccessKind.Read,
                                    Reference = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                }
                            )
                        ]),
                        new BlockStatement(null, [
                            new GoTo("if_0_then"),
                        ]),
                        new BlockStatement(null, [
                            new GoTo("if_0_end"),
                        ])
                    ),
                    new BlockStatement(null, [
                        new Label("if_0_then"),
                        new ReturnStatement(
                            null,
                            new LiteralExpression(null, LiteralExpressionKind.Integer, 1)
                            {
                                ReturnTypeMetadata = TypeMetadata.I32,
                            }
                        ),
                        new GoTo("if_0_end"),
                    ]),
                    new Label("if_0_end"),
                    new ReturnStatement(
                        null,
                        new LiteralExpression(null, LiteralExpressionKind.Integer, 0)
                        {
                            ReturnTypeMetadata = TypeMetadata.I32,
                        }
                    ),
                ])
            )
            {
                Metadata = new FunctionMetadata(
                    AccessModifierMetadata.Public,
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
            public test(a: bool, b: bool, c: bool): i32 {
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
        var expected = new SemanticTree(null, [
            new FunctionDeclaration(
                null,
                AccessModifier.Public,
                "test",
                [
                    new Parameter(null, "a", new Type(null, "bool") { Metadata = TypeMetadata.Bool })
                    {
                        Metadata = aParameter,
                    },
                    new Parameter(null, "b", new Type(null, "bool") { Metadata = TypeMetadata.Bool })
                    {
                        Metadata = bParameter,
                    },
                    new Parameter(null, "c", new Type(null, "bool") { Metadata = TypeMetadata.Bool })
                    {
                        Metadata = cParameter,
                    }
                ],
                new Type(null, "i32") { Metadata = TypeMetadata.I32 },
                new BlockStatement(null, [
                    new IfStatement(
                        null,
                        new ExpressionBlock([
                            new VariableDeclaration(
                                null,
                                "cond_1",
                                new Type(null, "bool") { Metadata = TypeMetadata.Bool },
                                new ExpressionBlock([
                                    new VariableDeclaration(
                                        null,
                                        "cond_0",
                                        new Type(null, "bool") { Metadata = TypeMetadata.Bool },
                                        new MemberAccessExpression(null, "a")
                                        {
                                            AccessKind = MemberAccessKind.Read,
                                            Reference = aParameter,
                                        }
                                    )
                                    {
                                        Metadata = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                    },
                                    new IfStatement(
                                        null,
                                        new MemberAccessExpression(null, "cond_0")
                                        {
                                            AccessKind = MemberAccessKind.Read,
                                            Reference = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                        },
                                        new BlockStatement(null, [
                                            new GoTo("if_1_then"),
                                        ]),
                                        new BlockStatement(null, [
                                            new GoTo("if_1_end"),
                                        ])
                                    ),
                                    new BlockStatement(null, [
                                        new Label("if_1_then"),
                                        new ExpressionStatement(
                                            null,
                                            new BinaryExpression(
                                                null,
                                                BinaryExpressionKind.Assignment,
                                                new MemberAccessExpression(null, "cond_0")
                                                {
                                                    AccessKind = MemberAccessKind.Write,
                                                    Reference = new VariableMetadata("cond_0", TypeMetadata.Bool),
                                                },
                                                new MemberAccessExpression(null, "b")
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
                                        null,
                                        new MemberAccessExpression(null, "cond_0")
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
                                null,
                                new MemberAccessExpression(null, "cond_1")
                                {
                                    AccessKind = MemberAccessKind.Read,
                                    Reference = new VariableMetadata("cond_1", TypeMetadata.Bool),
                                },
                                new BlockStatement(null, [
                                    new GoTo("if_2_then"),
                                ]),
                                new BlockStatement(null, [
                                    new GoTo("if_2_end"),
                                ])
                            ),
                            new BlockStatement(null, [
                                new Label("if_2_then"),
                                new ExpressionStatement(
                                    null,
                                    new BinaryExpression(
                                        null,
                                        BinaryExpressionKind.Assignment,
                                        new MemberAccessExpression(null, "cond_1")
                                        {
                                            AccessKind = MemberAccessKind.Write,
                                            Reference = new VariableMetadata("cond_1", TypeMetadata.Bool),
                                        },
                                        new MemberAccessExpression(null, "c")
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
                                null,
                                new MemberAccessExpression(null, "cond_1")
                                {
                                    AccessKind = MemberAccessKind.Read,
                                    Reference = new VariableMetadata("cond_1", TypeMetadata.Bool),
                                }
                            )
                        ]),
                        new BlockStatement(null, [
                            new GoTo("if_0_then"),
                        ]),
                        new BlockStatement(null, [
                            new GoTo("if_0_end"),
                        ])
                    ),
                    new BlockStatement(null, [
                        new Label("if_0_then"),
                        new ReturnStatement(
                            null,
                            new LiteralExpression(null, LiteralExpressionKind.Integer, 1)
                            {
                                ReturnTypeMetadata = TypeMetadata.I32,
                            }
                        ),
                        new GoTo("if_0_end"),
                    ]),
                    new Label("if_0_end"),
                    new ReturnStatement(
                        null,
                        new LiteralExpression(null, LiteralExpressionKind.Integer, 0)
                        {
                            ReturnTypeMetadata = TypeMetadata.I32,
                        }
                    ),
                ])
            )
            {
                Metadata = new FunctionMetadata(
                    AccessModifierMetadata.Public,
                    "test",
                    [aParameter, bParameter, cParameter],
                    new FunctionTypeMetadata([TypeMetadata.Bool, TypeMetadata.Bool, TypeMetadata.Bool], TypeMetadata.I32)
                ),
            }
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }
}