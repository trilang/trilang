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

public class ReplaceConditionalOperatorsTests
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

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = NamespaceMetadata.CreateRoot(builtInTypes);
        var aParameter = new ParameterMetadata(null, "a", builtInTypes.Bool);
        var bParameter = new ParameterMetadata(null, "b", builtInTypes.Bool);
        var expected = new SemanticTree(file, null, null, [], [
            new FunctionDeclaration(
                null,
                AccessModifier.Public,
                "test",
                [
                    new Parameter(null, "a", new TypeRef(null, "bool") { Metadata = builtInTypes.Bool })
                    {
                        Metadata = aParameter,
                    },
                    new Parameter(null, "b", new TypeRef(null, "bool") { Metadata = builtInTypes.Bool })
                    {
                        Metadata = bParameter,
                    },
                ],
                new TypeRef(null, "i32") { Metadata = builtInTypes.I32 },
                new BlockStatement(null, [
                    new IfStatement(
                        null,
                        new ExpressionBlock([
                            new VariableDeclaration(
                                null,
                                "cond_0",
                                new TypeRef(null, "bool") { Metadata = builtInTypes.Bool },
                                new MemberAccessExpression(null, "a")
                                {
                                    AccessKind = MemberAccessKind.Read,
                                    Reference = aParameter,
                                }
                            )
                            {
                                Metadata = new VariableMetadata(null, "cond_0", builtInTypes.Bool),
                            },
                            new IfStatement(
                                null,
                                new MemberAccessExpression(null, "cond_0")
                                {
                                    AccessKind = MemberAccessKind.Read,
                                    Reference = new VariableMetadata(null, "cond_0", builtInTypes.Bool),
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
                                            Reference = new VariableMetadata(null, "cond_0", builtInTypes.Bool),
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
                                    Reference = new VariableMetadata(null, "cond_0", builtInTypes.Bool),
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
                                ReturnTypeMetadata = builtInTypes.I32,
                            }
                        ),
                        new GoTo("if_0_end"),
                    ]),
                    new Label("if_0_end"),
                    new ReturnStatement(
                        null,
                        new LiteralExpression(null, LiteralExpressionKind.Integer, 0)
                        {
                            ReturnTypeMetadata = builtInTypes.I32,
                        }
                    ),
                ])
            )
            {
                Metadata = new FunctionMetadata(
                    null,
                    AccessModifierMetadata.Public,
                    "test",
                    [aParameter, bParameter],
                    CreateFunctionType([builtInTypes.Bool, builtInTypes.Bool],
                        builtInTypes.I32,
                        rootNamespace))
                {
                    Namespace = rootNamespace,
                },
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

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = NamespaceMetadata.CreateRoot(builtInTypes);
        var aParameter = new ParameterMetadata(null, "a", builtInTypes.Bool);
        var bParameter = new ParameterMetadata(null, "b", builtInTypes.Bool);
        var expected = new SemanticTree(file, null, null, [], [
            new FunctionDeclaration(
                null,
                AccessModifier.Public,
                "test",
                [
                    new Parameter(null, "a", new TypeRef(null, "bool") { Metadata = builtInTypes.Bool })
                    {
                        Metadata = aParameter,
                    },
                    new Parameter(null, "b", new TypeRef(null, "bool") { Metadata = builtInTypes.Bool })
                    {
                        Metadata = bParameter,
                    },
                ],
                new TypeRef(null, "i32") { Metadata = builtInTypes.I32 },
                new BlockStatement(null, [
                    new IfStatement(
                        null,
                        new ExpressionBlock([
                            new VariableDeclaration(
                                null,
                                "cond_0",
                                new TypeRef(null, "bool") { Metadata = builtInTypes.Bool },
                                new MemberAccessExpression(null, "a")
                                {
                                    AccessKind = MemberAccessKind.Read,
                                    Reference = aParameter,
                                }
                            )
                            {
                                Metadata = new VariableMetadata(null, "cond_0", builtInTypes.Bool),
                            },
                            new IfStatement(
                                null,
                                new UnaryExpression(
                                    null,
                                    UnaryExpressionKind.LogicalNot,
                                    new MemberAccessExpression(null, "cond_0")
                                    {
                                        AccessKind = MemberAccessKind.Read,
                                        Reference = new VariableMetadata(null, "cond_0", builtInTypes.Bool),
                                    }
                                )
                                {
                                    ReturnTypeMetadata = builtInTypes.Bool,
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
                                            Reference = new VariableMetadata(null, "cond_0", builtInTypes.Bool),
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
                                    Reference = new VariableMetadata(null, "cond_0", builtInTypes.Bool),
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
                                ReturnTypeMetadata = builtInTypes.I32,
                            }
                        ),
                        new GoTo("if_0_end"),
                    ]),
                    new Label("if_0_end"),
                    new ReturnStatement(
                        null,
                        new LiteralExpression(null, LiteralExpressionKind.Integer, 0)
                        {
                            ReturnTypeMetadata = builtInTypes.I32,
                        }
                    ),
                ])
            )
            {
                Metadata = new FunctionMetadata(
                    null,
                    AccessModifierMetadata.Public,
                    "test",
                    [aParameter, bParameter],
                    CreateFunctionType([builtInTypes.Bool, builtInTypes.Bool],
                        builtInTypes.I32,
                        rootNamespace))
                {
                    Namespace = rootNamespace,
                },
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

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = NamespaceMetadata.CreateRoot(builtInTypes);
        var aParameter = new ParameterMetadata(null, "a", builtInTypes.Bool);
        var bParameter = new ParameterMetadata(null, "b", builtInTypes.Bool);
        var cParameter = new ParameterMetadata(null, "c", builtInTypes.Bool);
        var expected = new SemanticTree(file, null, null, [], [
            new FunctionDeclaration(
                null,
                AccessModifier.Public,
                "test",
                [
                    new Parameter(null, "a", new TypeRef(null, "bool") { Metadata = builtInTypes.Bool })
                    {
                        Metadata = aParameter,
                    },
                    new Parameter(null, "b", new TypeRef(null, "bool") { Metadata = builtInTypes.Bool })
                    {
                        Metadata = bParameter,
                    },
                    new Parameter(null, "c", new TypeRef(null, "bool") { Metadata = builtInTypes.Bool })
                    {
                        Metadata = cParameter,
                    }
                ],
                new TypeRef(null, "i32") { Metadata = builtInTypes.I32 },
                new BlockStatement(null, [
                    new IfStatement(
                        null,
                        new ExpressionBlock([
                            new VariableDeclaration(
                                null,
                                "cond_1",
                                new TypeRef(null, "bool") { Metadata = builtInTypes.Bool },
                                new ExpressionBlock([
                                    new VariableDeclaration(
                                        null,
                                        "cond_0",
                                        new TypeRef(null, "bool") { Metadata = builtInTypes.Bool },
                                        new MemberAccessExpression(null, "a")
                                        {
                                            AccessKind = MemberAccessKind.Read,
                                            Reference = aParameter,
                                        }
                                    )
                                    {
                                        Metadata = new VariableMetadata(null, "cond_0", builtInTypes.Bool),
                                    },
                                    new IfStatement(
                                        null,
                                        new MemberAccessExpression(null, "cond_0")
                                        {
                                            AccessKind = MemberAccessKind.Read,
                                            Reference = new VariableMetadata(null, "cond_0", builtInTypes.Bool),
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
                                                    Reference = new VariableMetadata(null, "cond_0", builtInTypes.Bool),
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
                                            Reference = new VariableMetadata(null, "cond_0", builtInTypes.Bool),
                                        }
                                    )
                                ])
                            )
                            {
                                Metadata = new VariableMetadata(null, "cond_1", builtInTypes.Bool),
                            },
                            new IfStatement(
                                null,
                                new MemberAccessExpression(null, "cond_1")
                                {
                                    AccessKind = MemberAccessKind.Read,
                                    Reference = new VariableMetadata(null, "cond_1", builtInTypes.Bool),
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
                                            Reference = new VariableMetadata(null, "cond_1", builtInTypes.Bool),
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
                                    Reference = new VariableMetadata(null, "cond_1", builtInTypes.Bool),
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
                                ReturnTypeMetadata = builtInTypes.I32,
                            }
                        ),
                        new GoTo("if_0_end"),
                    ]),
                    new Label("if_0_end"),
                    new ReturnStatement(
                        null,
                        new LiteralExpression(null, LiteralExpressionKind.Integer, 0)
                        {
                            ReturnTypeMetadata = builtInTypes.I32,
                        }
                    ),
                ])
            )
            {
                Metadata = new FunctionMetadata(
                    null,
                    AccessModifierMetadata.Public,
                    "test",
                    [aParameter, bParameter, cParameter],
                    CreateFunctionType([builtInTypes.Bool, builtInTypes.Bool, builtInTypes.Bool],
                        builtInTypes.I32,
                        rootNamespace))
                {
                    Namespace = rootNamespace,
                },
            }
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }
}