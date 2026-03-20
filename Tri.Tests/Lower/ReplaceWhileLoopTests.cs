using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Lower;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using static Tri.Tests.Factory;

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

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var semanticTree = semanticTrees.Single();

        var lowering = new Lowering(builtInTypes);
        lowering.Lower(semanticTree, LoweringOptions.Default);

        return semanticTree;
    }

    [Test]
    public void ReplaceWhileLoopTest()
    {
        var tree = Parse(
            """
            namespace Test1;

            public test(a: i32): i32 {
                a = 0;

                while (a < 10) {
                    a += 1;
                }

                return a;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = NamespaceMetadata.CreateRoot(builtInTypes);
        var test1Ns = rootNamespace.CreateChild(["Test1"]);
        var parameterMetadata = new ParameterMetadata(null, "a", builtInTypes.I32);
        var expected = new SemanticTree(
            file,
            null,
            new Namespace(null, ["Test1"]),
            [],
            [
                new FunctionDeclaration(
                    null,
                    AccessModifier.Public,
                    "test",
                    [
                        new Parameter(null, "a", new TypeRef(null, ["i32"]) { Metadata = builtInTypes.I32 })
                        {
                            Metadata = parameterMetadata,
                        }
                    ],
                    new TypeRef(null, ["i32"]) { Metadata = builtInTypes.I32 },
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
                                    ReturnTypeMetadata = builtInTypes.I32
                                }
                            )
                            {
                                ReturnTypeMetadata = builtInTypes.I32
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
                                        ReturnTypeMetadata = builtInTypes.I32
                                    }
                                )
                                {
                                    ReturnTypeMetadata = builtInTypes.Bool
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
                                                ReturnTypeMetadata = builtInTypes.I32
                                            }
                                        )
                                        {
                                            ReturnTypeMetadata = builtInTypes.I32
                                        }
                                    )
                                    {
                                        ReturnTypeMetadata = builtInTypes.I32
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
                        CreateFunctionType([builtInTypes.I32], builtInTypes.I32, rootNamespace)
                    )
                    {
                        Namespace = test1Ns,
                    }
                }
            ]);


        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void ReplaceNestedWhileLoopTest()
    {
        var tree = Parse(
            """
            namespace Test1;

            public test(a: i32): i32 {
                a = 0;

                while (a < 10) {
                    while (true) { }

                    a += 1;
                }

                return a;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = NamespaceMetadata.CreateRoot(builtInTypes);
        var test1Ns = rootNamespace.CreateChild(["Test1"]);
        var parameterMetadata = new ParameterMetadata(null, "a", builtInTypes.I32);
        var expected = new SemanticTree(
            file,
            null,
            new Namespace(null, ["Test1"]),
            [],
            [
                new FunctionDeclaration(
                    null,
                    AccessModifier.Public,
                    "test",
                    [
                        new Parameter(null, "a", new TypeRef(null, ["i32"]) { Metadata = builtInTypes.I32 })
                        {
                            Metadata = parameterMetadata,
                        }
                    ],
                    new TypeRef(null, ["i32"]) { Metadata = builtInTypes.I32 },
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
                                    ReturnTypeMetadata = builtInTypes.I32
                                }
                            )
                            {
                                ReturnTypeMetadata = builtInTypes.I32
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
                                        ReturnTypeMetadata = builtInTypes.I32
                                    }
                                )
                                {
                                    ReturnTypeMetadata = builtInTypes.Bool
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
                                            ReturnTypeMetadata = builtInTypes.Bool
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
                                                ReturnTypeMetadata = builtInTypes.I32
                                            }
                                        )
                                        {
                                            ReturnTypeMetadata = builtInTypes.I32
                                        }
                                    )
                                    {
                                        ReturnTypeMetadata = builtInTypes.I32
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
                        CreateFunctionType([builtInTypes.I32], builtInTypes.I32, rootNamespace)
                    )
                    {
                        Namespace = test1Ns,
                    }
                }
            ]);


        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void ReplaceBreakInWhileLoopTest()
    {
        var tree = Parse(
            """
            namespace Test1;

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

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = NamespaceMetadata.CreateRoot(builtInTypes);
        var test1Ns = rootNamespace.CreateChild(["Test1"]);
        var parameterMetadata = new ParameterMetadata(null, "a", builtInTypes.I32);
        var expected = new SemanticTree(
            file,
            null,
            new Namespace(null, ["Test1"]),
            [],
            [
                new FunctionDeclaration(
                    null,
                    AccessModifier.Public,
                    "test",
                    [
                        new Parameter(null, "a", new TypeRef(null, ["i32"]) { Metadata = builtInTypes.I32 })
                        {
                            Metadata = parameterMetadata,
                        }
                    ],
                    new TypeRef(null, ["i32"]) { Metadata = builtInTypes.I32 },
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
                                    ReturnTypeMetadata = builtInTypes.I32
                                }
                            )
                            {
                                ReturnTypeMetadata = builtInTypes.I32
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
                                        ReturnTypeMetadata = builtInTypes.I32
                                    }
                                )
                                {
                                    ReturnTypeMetadata = builtInTypes.Bool
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
                                                ReturnTypeMetadata = builtInTypes.I32
                                            }
                                        )
                                        {
                                            ReturnTypeMetadata = builtInTypes.I32
                                        }
                                    )
                                    {
                                        ReturnTypeMetadata = builtInTypes.I32
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
                                            ReturnTypeMetadata = builtInTypes.I32
                                        }
                                    )
                                    {
                                        ReturnTypeMetadata = builtInTypes.Bool
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
                        CreateFunctionType([builtInTypes.I32], builtInTypes.I32, rootNamespace)
                    )
                    {
                        Namespace = test1Ns,
                    }
                }
            ]);


        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void ReplaceContinueInWhileLoopTest()
    {
        var tree = Parse(
            """
            namespace Test1;

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

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = NamespaceMetadata.CreateRoot(builtInTypes);
        var test1Ns = rootNamespace.CreateChild(["Test1"]);
        var parameterMetadata = new ParameterMetadata(null, "a", builtInTypes.I32);
        var expected = new SemanticTree(
            file,
            null,
            new Namespace(null, ["Test1"]),
            [],
            [
                new FunctionDeclaration(
                    null,
                    AccessModifier.Public,
                    "test",
                    [
                        new Parameter(null, "a", new TypeRef(null, ["i32"]) { Metadata = builtInTypes.I32 })
                        {
                            Metadata = parameterMetadata,
                        }
                    ],
                    new TypeRef(null, ["i32"]) { Metadata = builtInTypes.I32 },
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
                                    ReturnTypeMetadata = builtInTypes.I32
                                }
                            )
                            {
                                ReturnTypeMetadata = builtInTypes.I32
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
                                        ReturnTypeMetadata = builtInTypes.I32
                                    }
                                )
                                {
                                    ReturnTypeMetadata = builtInTypes.Bool
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
                                                ReturnTypeMetadata = builtInTypes.I32
                                            }
                                        )
                                        {
                                            ReturnTypeMetadata = builtInTypes.I32
                                        }
                                    )
                                    {
                                        ReturnTypeMetadata = builtInTypes.I32
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
                                            ReturnTypeMetadata = builtInTypes.I32
                                        }
                                    )
                                    {
                                        ReturnTypeMetadata = builtInTypes.Bool
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
                        CreateFunctionType([builtInTypes.I32], builtInTypes.I32, rootNamespace)
                    )
                    {
                        Namespace = test1Ns,
                    }
                }
            ]);


        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }
}