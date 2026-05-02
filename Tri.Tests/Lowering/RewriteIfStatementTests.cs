using Trilang.Metadata;
using Trilang.Semantics.Model;
using static Tri.Tests.Factory;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Lowering;

public class RewriteIfStatementTests
{
    [Test]
    public void RewriteIfElseStatementTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(a: i32): i32 {
                if (a >= 0) {
                    return a;
                } else {
                    return -a;
                }
            }
            """);
        var (tree, diagnostics, _) = Lower(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
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
                        new Parameter(null, "a", new TypeRef(null, null, ["i32"]) { Metadata = builtInTypes.I32 })
                        {
                            Metadata = parameterMetadata,
                        }
                    ],
                    new TypeRef(null, null, ["i32"]) { Metadata = builtInTypes.I32 },
                    new BlockStatement(null, [
                        new IfStatement(
                            null,
                            new BinaryExpression(
                                null,
                                BinaryExpressionKind.GreaterThanOrEqual,
                                new MemberAccessExpression(null, "a")
                                {
                                    Reference = parameterMetadata,
                                    AccessKind = MemberAccessKind.Read,
                                },
                                new LiteralExpression(null, LiteralExpressionKind.Integer, 0)
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
                                new GoTo("if_0_else")
                            ])
                        ),
                        new BlockStatement(null, [
                            new Label("if_0_then"),
                            new ReturnStatement(
                                null,
                                new MemberAccessExpression(null, "a")
                                {
                                    Reference = parameterMetadata,
                                    AccessKind = MemberAccessKind.Read,
                                }
                            ),
                            new GoTo("if_0_end"),
                        ]),
                        new BlockStatement(null, [
                            new Label("if_0_else"),
                            new ReturnStatement(
                                null,
                                new UnaryExpression(
                                    null,
                                    UnaryExpressionKind.UnaryMinus,
                                    new MemberAccessExpression(null, "a")
                                    {
                                        Reference = parameterMetadata,
                                        AccessKind = MemberAccessKind.Read,
                                    }
                                )
                                {
                                    ReturnTypeMetadata = builtInTypes.I32
                                }
                            ),
                            new GoTo("if_0_end"),
                        ]),
                        new Label("if_0_end"),
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
                        Namespace = rootNamespace,
                    }
                }
            ]);


        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void RewriteIfWithoutElseStatementTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(a: i32): i32 {
                if (a >= 0) {
                    return a;
                }

                return -a;
            }
            """);
        var (tree, diagnostics, _) = Lower(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
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
                        new Parameter(null, "a", new TypeRef(null, null, ["i32"]) { Metadata = builtInTypes.I32 })
                        {
                            Metadata = parameterMetadata,
                        }
                    ],
                    new TypeRef(null, null, ["i32"]) { Metadata = builtInTypes.I32 },
                    new BlockStatement(null, [
                        new IfStatement(
                            null,
                            new BinaryExpression(
                                null,
                                BinaryExpressionKind.GreaterThanOrEqual,
                                new MemberAccessExpression(null, "a")
                                {
                                    Reference = parameterMetadata,
                                    AccessKind = MemberAccessKind.Read,
                                },
                                new LiteralExpression(null, LiteralExpressionKind.Integer, 0)
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
                                new GoTo("if_0_end")
                            ])
                        ),
                        new BlockStatement(null, [
                            new Label("if_0_then"),
                            new ReturnStatement(
                                null,
                                new MemberAccessExpression(null, "a")
                                {
                                    Reference = parameterMetadata,
                                    AccessKind = MemberAccessKind.Read,
                                }
                            ),
                            new GoTo("if_0_end"),
                        ]),
                        new Label("if_0_end"),
                        new ReturnStatement(
                            null,
                            new UnaryExpression(
                                null,
                                UnaryExpressionKind.UnaryMinus,
                                new MemberAccessExpression(null, "a")
                                {
                                    Reference = parameterMetadata,
                                    AccessKind = MemberAccessKind.Read,
                                }
                            )
                            {
                                ReturnTypeMetadata = builtInTypes.I32
                            }
                        ),
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
                        Namespace = rootNamespace,
                    }
                }
            ]);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }
}