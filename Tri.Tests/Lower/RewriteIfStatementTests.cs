using Trilang.Lower;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using Type = Trilang.Semantics.Model.Type;

namespace Tri.Tests.Lower;

public class RewriteIfStatementTests
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
        var expected = new SemanticTree(null, [
            new FunctionDeclaration(
                null,
                "test",
                [
                    new Parameter(null, "a", new Type(null, "i32") { Metadata = TypeMetadata.I32 })
                    {
                        Metadata = parameterMetadata,
                    }
                ],
                new Type(null, "i32") { Metadata = TypeMetadata.I32 },
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
                                ReturnTypeMetadata = TypeMetadata.I32
                            }
                        ),
                        new GoTo("if_0_end"),
                    ]),
                    new Label("if_0_end"),
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

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
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
        var expected = new SemanticTree(null, [
            new FunctionDeclaration(
                null,
                "test",
                [
                    new Parameter(null, "a", new Type(null, "i32") { Metadata = TypeMetadata.I32 })
                    {
                        Metadata = parameterMetadata,
                    }
                ],
                new Type(null, "i32") { Metadata = TypeMetadata.I32 },
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

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }
}