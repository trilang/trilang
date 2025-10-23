using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Lower;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using Type = Trilang.Semantics.Model.Type;

namespace Tri.Tests.Lower;

public class ReplaceWhileLoopTests
{
    private static readonly SourceFile file = new SourceFile("test.tri");

    private static SemanticTree Parse(string code)
    {
        var diagnostics = new DiagnosticCollection();

        var lexer = new Lexer();
        var lexerOptions = new LexerOptions(new LexerDiagnosticReporter(diagnostics, file));
        var tokens = lexer.Tokenize(code, lexerOptions);

        var parser = new Parser();
        var parserOptions = new ParserOptions(file, new ParserDiagnosticReporter(diagnostics, file));
        var tree = parser.Parse(tokens, parserOptions);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        return semanticTrees.Single();
    }

    [Test]
    public void ReplaceWhileLoopTest()
    {
        var tree = Parse(
            """
            public test(a: i32): i32 {
                a = 0;

                while (a < 10) {
                    a += 1;
                }

                return a;
            }
            """);
        var parameterMetadata = new ParameterMetadata(null, "a", TypeMetadata.I32);
        var expected = new SemanticTree(file, null, [
            new FunctionDeclaration(
                null,
                AccessModifier.Public,
                "test",
                [
                    new Parameter(null, "a", new Type(null, "i32") { Metadata = TypeMetadata.I32 })
                    {
                        Metadata = parameterMetadata,
                    }
                ],
                new Type(null, "i32") { Metadata = TypeMetadata.I32 },
                new BlockStatement(null, [
                    new ExpressionStatement(
                        null,
                        new BinaryExpression(
                            null,
                            BinaryExpressionKind.Assignment,
                            new MemberAccessExpression(null, "a")
                            {
                                Reference = parameterMetadata,
                                AccessKind = MemberAccessKind.Write,
                            },
                            new LiteralExpression(null, LiteralExpressionKind.Integer, 0)
                            {
                                ReturnTypeMetadata = TypeMetadata.I32
                            }
                        )
                        {
                            ReturnTypeMetadata = TypeMetadata.I32
                        }
                    ),
                    new BlockStatement(null, [
                        new GoTo("loop_0_start"),
                        new Label("loop_0_start"),
                        new IfStatement(
                            null,
                            new BinaryExpression(
                                null,
                                BinaryExpressionKind.LessThan,
                                new MemberAccessExpression(null, "a")
                                {
                                    Reference = parameterMetadata,
                                    AccessKind = MemberAccessKind.Read,
                                },
                                new LiteralExpression(null, LiteralExpressionKind.Integer, 10)
                                {
                                    ReturnTypeMetadata = TypeMetadata.I32
                                }
                            )
                            {
                                ReturnTypeMetadata = TypeMetadata.Bool
                            },
                            new BlockStatement(null, [
                                new GoTo("if_0_then")
                            ]),
                            new BlockStatement(null, [
                                new GoTo("loop_0_end")
                            ])
                        ),
                        new BlockStatement(null, [
                            new Label("if_0_then"),
                            new ExpressionStatement(
                                null,
                                new BinaryExpression(
                                    null,
                                    BinaryExpressionKind.Assignment,
                                    new MemberAccessExpression(null, "a")
                                    {
                                        Reference = parameterMetadata,
                                        AccessKind = MemberAccessKind.Write,
                                    },
                                    new BinaryExpression(
                                        null,
                                        BinaryExpressionKind.Addition,
                                        new MemberAccessExpression(null, "a")
                                        {
                                            Reference = parameterMetadata,
                                            AccessKind = MemberAccessKind.Read,
                                        },
                                        new LiteralExpression(null, LiteralExpressionKind.Integer, 1)
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
                            new GoTo("loop_0_start"),
                        ]),
                        new Label("loop_0_end"),
                    ]),
                    new ReturnStatement(
                        null,
                        new MemberAccessExpression(null, "a")
                        {
                            Reference = parameterMetadata,
                            AccessKind = MemberAccessKind.Read,
                        }
                    )
                ])
            )
            {
                Metadata = new FunctionMetadata(
                    null,
                    AccessModifierMetadata.Public,
                    "test",
                    [parameterMetadata],
                    new FunctionTypeMetadata(null, [TypeMetadata.I32], TypeMetadata.I32)
                )
            }
        ]);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void ReplaceNestedWhileLoopTest()
    {
        var tree = Parse(
            """
            public test(a: i32): i32 {
                a = 0;

                while (a < 10) {
                    while (true) { }

                    a += 1;
                }

                return a;
            }
            """);
        var parameterMetadata = new ParameterMetadata(null, "a", TypeMetadata.I32);
        var expected = new SemanticTree(file, null, [
            new FunctionDeclaration(
                null,
                AccessModifier.Public,
                "test",
                [
                    new Parameter(null, "a", new Type(null, "i32") { Metadata = TypeMetadata.I32 })
                    {
                        Metadata = parameterMetadata,
                    }
                ],
                new Type(null, "i32") { Metadata = TypeMetadata.I32 },
                new BlockStatement(null, [
                    new ExpressionStatement(
                        null,
                        new BinaryExpression(
                            null,
                            BinaryExpressionKind.Assignment,
                            new MemberAccessExpression(null, "a")
                            {
                                Reference = parameterMetadata,
                                AccessKind = MemberAccessKind.Write,
                            },
                            new LiteralExpression(null, LiteralExpressionKind.Integer, 0)
                            {
                                ReturnTypeMetadata = TypeMetadata.I32
                            }
                        )
                        {
                            ReturnTypeMetadata = TypeMetadata.I32
                        }
                    ),
                    new BlockStatement(null, [
                        new GoTo("loop_0_start"),
                        new Label("loop_0_start"),
                        new IfStatement(
                            null,
                            new BinaryExpression(
                                null,
                                BinaryExpressionKind.LessThan,
                                new MemberAccessExpression(null, "a")
                                {
                                    Reference = parameterMetadata,
                                    AccessKind = MemberAccessKind.Read,
                                },
                                new LiteralExpression(null, LiteralExpressionKind.Integer, 10)
                                {
                                    ReturnTypeMetadata = TypeMetadata.I32
                                }
                            )
                            {
                                ReturnTypeMetadata = TypeMetadata.Bool
                            },
                            new BlockStatement(null, [
                                new GoTo("if_0_then"),
                            ]),
                            new BlockStatement(null, [
                                new GoTo("loop_0_end")
                            ])
                        ),
                        new BlockStatement(null, [
                            new Label("if_0_then"),
                            new BlockStatement(null, [
                                new GoTo("loop_1_start"),
                                new Label("loop_1_start"),
                                new IfStatement(
                                    null,
                                    new LiteralExpression(null, LiteralExpressionKind.Boolean, true)
                                    {
                                        ReturnTypeMetadata = TypeMetadata.Bool
                                    },
                                    new BlockStatement(null, [
                                        new GoTo("loop_1_start"),
                                    ]),
                                    new BlockStatement(null, [
                                        new GoTo("loop_1_end"),
                                    ])
                                ),
                                new Label("loop_1_end"),
                            ]),
                            new ExpressionStatement(
                                null,
                                new BinaryExpression(
                                    null,
                                    BinaryExpressionKind.Assignment,
                                    new MemberAccessExpression(null, "a")
                                    {
                                        Reference = parameterMetadata,
                                        AccessKind = MemberAccessKind.Write,
                                    },
                                    new BinaryExpression(
                                        null,
                                        BinaryExpressionKind.Addition,
                                        new MemberAccessExpression(null, "a")
                                        {
                                            Reference = parameterMetadata,
                                            AccessKind = MemberAccessKind.Read,
                                        },
                                        new LiteralExpression(null, LiteralExpressionKind.Integer, 1)
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
                            new GoTo("loop_0_start"),
                        ]),
                        new Label("loop_0_end"),
                    ]),
                    new ReturnStatement(
                        null,
                        new MemberAccessExpression(null, "a")
                        {
                            Reference = parameterMetadata,
                            AccessKind = MemberAccessKind.Read,
                        }
                    )
                ])
            )
            {
                Metadata = new FunctionMetadata(
                    null,
                    AccessModifierMetadata.Public,
                    "test",
                    [parameterMetadata],
                    new FunctionTypeMetadata(null, [TypeMetadata.I32], TypeMetadata.I32)
                )
            }
        ]);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void ReplaceBreakInWhileLoopTest()
    {
        var tree = Parse(
            """
            public test(a: i32): i32 {
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
        var parameterMetadata = new ParameterMetadata(null, "a", TypeMetadata.I32);
        var expected = new SemanticTree(file, null, [
            new FunctionDeclaration(
                null,
                AccessModifier.Public,
                "test",
                [
                    new Parameter(null, "a", new Type(null, "i32") { Metadata = TypeMetadata.I32 })
                    {
                        Metadata = parameterMetadata,
                    }
                ],
                new Type(null, "i32") { Metadata = TypeMetadata.I32 },
                new BlockStatement(null, [
                    new ExpressionStatement(
                        null,
                        new BinaryExpression(
                            null,
                            BinaryExpressionKind.Assignment,
                            new MemberAccessExpression(null, "a")
                            {
                                Reference = parameterMetadata,
                                AccessKind = MemberAccessKind.Write,
                            },
                            new LiteralExpression(null, LiteralExpressionKind.Integer, 0)
                            {
                                ReturnTypeMetadata = TypeMetadata.I32
                            }
                        )
                        {
                            ReturnTypeMetadata = TypeMetadata.I32
                        }
                    ),
                    new BlockStatement(null, [
                        new GoTo("loop_0_start"),
                        new Label("loop_0_start"),
                        new IfStatement(
                            null,
                            new BinaryExpression(
                                null,
                                BinaryExpressionKind.LessThan,
                                new MemberAccessExpression(null, "a")
                                {
                                    Reference = parameterMetadata,
                                    AccessKind = MemberAccessKind.Read,
                                },
                                new LiteralExpression(null, LiteralExpressionKind.Integer, 10)
                                {
                                    ReturnTypeMetadata = TypeMetadata.I32
                                }
                            )
                            {
                                ReturnTypeMetadata = TypeMetadata.Bool
                            },
                            new BlockStatement(null, [
                                new GoTo("if_0_then")
                            ]),
                            new BlockStatement(null, [
                                new GoTo("loop_0_end")
                            ])
                        ),
                        new BlockStatement(null, [
                            new Label("if_0_then"),
                            new ExpressionStatement(
                                null,
                                new BinaryExpression(
                                    null,
                                    BinaryExpressionKind.Assignment,
                                    new MemberAccessExpression(null, "a")
                                    {
                                        Reference = parameterMetadata,
                                        AccessKind = MemberAccessKind.Write,
                                    },
                                    new BinaryExpression(
                                        null,
                                        BinaryExpressionKind.Addition,
                                        new MemberAccessExpression(null, "a")
                                        {
                                            Reference = parameterMetadata,
                                            AccessKind = MemberAccessKind.Read,
                                        },
                                        new LiteralExpression(null, LiteralExpressionKind.Integer, 1)
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
                            new IfStatement(
                                null,
                                new BinaryExpression(
                                    null,
                                    BinaryExpressionKind.Equality,
                                    new MemberAccessExpression(null, "a")
                                    {
                                        Reference = parameterMetadata,
                                        AccessKind = MemberAccessKind.Read,
                                    },
                                    new LiteralExpression(null, LiteralExpressionKind.Integer, 5)
                                    {
                                        ReturnTypeMetadata = TypeMetadata.I32
                                    }
                                )
                                {
                                    ReturnTypeMetadata = TypeMetadata.Bool
                                },
                                new BlockStatement(null, [
                                    new GoTo("loop_0_end"),
                                ]),
                                new BlockStatement(null, [
                                    new GoTo("if_1_end"),
                                ])
                            ),
                            new Label("if_1_end"),
                            new GoTo("loop_0_start"),
                        ]),
                        new Label("loop_0_end"),
                    ]),
                    new ReturnStatement(
                        null,
                        new MemberAccessExpression(null, "a")
                        {
                            Reference = parameterMetadata,
                            AccessKind = MemberAccessKind.Read,
                        }
                    )
                ])
            )
            {
                Metadata = new FunctionMetadata(
                    null,
                    AccessModifierMetadata.Public,
                    "test",
                    [parameterMetadata],
                    new FunctionTypeMetadata(null, [TypeMetadata.I32], TypeMetadata.I32)
                )
            }
        ]);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void ReplaceContinueInWhileLoopTest()
    {
        var tree = Parse(
            """
            public test(a: i32): i32 {
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
        var parameterMetadata = new ParameterMetadata(null, "a", TypeMetadata.I32);
        var expected = new SemanticTree(file, null, [
            new FunctionDeclaration(
                null,
                AccessModifier.Public,
                "test",
                [
                    new Parameter(null, "a", new Type(null, "i32") { Metadata = TypeMetadata.I32 })
                    {
                        Metadata = parameterMetadata,
                    }
                ],
                new Type(null, "i32") { Metadata = TypeMetadata.I32 },
                new BlockStatement(null, [
                    new ExpressionStatement(
                        null,
                        new BinaryExpression(
                            null,
                            BinaryExpressionKind.Assignment,
                            new MemberAccessExpression(null, "a")
                            {
                                Reference = parameterMetadata,
                                AccessKind = MemberAccessKind.Write,
                            },
                            new LiteralExpression(null, LiteralExpressionKind.Integer, 0)
                            {
                                ReturnTypeMetadata = TypeMetadata.I32
                            }
                        )
                        {
                            ReturnTypeMetadata = TypeMetadata.I32
                        }
                    ),
                    new BlockStatement(null, [
                        new GoTo("loop_0_start"),
                        new Label("loop_0_start"),
                        new IfStatement(
                            null,
                            new BinaryExpression(
                                null,
                                BinaryExpressionKind.LessThan,
                                new MemberAccessExpression(null, "a")
                                {
                                    Reference = parameterMetadata,
                                    AccessKind = MemberAccessKind.Read,
                                },
                                new LiteralExpression(null, LiteralExpressionKind.Integer, 10)
                                {
                                    ReturnTypeMetadata = TypeMetadata.I32
                                }
                            )
                            {
                                ReturnTypeMetadata = TypeMetadata.Bool
                            },
                            new BlockStatement(null, [
                                new GoTo("if_0_then")
                            ]),
                            new BlockStatement(null, [
                                new GoTo("loop_0_end")
                            ])
                        ),
                        new BlockStatement(null, [
                            new Label("if_0_then"),
                            new ExpressionStatement(
                                null,
                                new BinaryExpression(
                                    null,
                                    BinaryExpressionKind.Assignment,
                                    new MemberAccessExpression(null, "a")
                                    {
                                        Reference = parameterMetadata,
                                        AccessKind = MemberAccessKind.Write,
                                    },
                                    new BinaryExpression(
                                        null,
                                        BinaryExpressionKind.Addition,
                                        new MemberAccessExpression(null, "a")
                                        {
                                            Reference = parameterMetadata,
                                            AccessKind = MemberAccessKind.Read,
                                        },
                                        new LiteralExpression(null, LiteralExpressionKind.Integer, 1)
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
                            new IfStatement(
                                null,
                                new BinaryExpression(
                                    null,
                                    BinaryExpressionKind.Equality,
                                    new MemberAccessExpression(null, "a")
                                    {
                                        Reference = parameterMetadata,
                                        AccessKind = MemberAccessKind.Read,
                                    },
                                    new LiteralExpression(null, LiteralExpressionKind.Integer, 5)
                                    {
                                        ReturnTypeMetadata = TypeMetadata.I32
                                    }
                                )
                                {
                                    ReturnTypeMetadata = TypeMetadata.Bool
                                },
                                new BlockStatement(null, [
                                    new GoTo("loop_0_start"),
                                ]),
                                new BlockStatement(null, [
                                    new GoTo("if_1_end"),
                                ])
                            ),
                            new Label("if_1_end"),
                            new GoTo("loop_0_start"),
                        ]),
                        new Label("loop_0_end"),
                    ]),
                    new ReturnStatement(
                        null,
                        new MemberAccessExpression(null, "a")
                        {
                            Reference = parameterMetadata,
                            AccessKind = MemberAccessKind.Read,
                        }
                    )
                ])
            )
            {
                Metadata = new FunctionMetadata(
                    null,
                    AccessModifierMetadata.Public,
                    "test",
                    [parameterMetadata],
                    new FunctionTypeMetadata(null, [TypeMetadata.I32], TypeMetadata.I32)
                )
            }
        ]);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }
}