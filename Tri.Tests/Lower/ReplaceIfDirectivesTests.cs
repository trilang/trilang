using Trilang.Lower;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Lower;

public class ReplaceIfDirectivesTests
{
    private static SyntaxTree Parse(string code, IEnumerable<string> directives)
    {
        var parser = new Parser();
        var tree = parser.Parse(code);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, new SemanticAnalysisOptions(directives));

        return tree;
    }

    [Test]
    public void ReplaceIfDirectiveWithThenDeclarationsTest()
    {
        const string code =
            """
            #if D1

            public type Type1 { }

            #else

            public type Type2 { }

            #endif

            public type Type3 { }
            """;

        var directives = new[] { "D1" };
        var tree = Parse(code, directives);

        var lowering = new Lowering();
        lowering.Lower(tree, new LoweringOptions(directives));

        var expected = new SyntaxTree([
            new TypeDeclarationNode(AccessModifier.Public, "Type1", [], [], [], [], [])
            {
                Metadata = new TypeMetadata("Type1"),
            },
            new TypeDeclarationNode(AccessModifier.Public, "Type3", [], [], [], [], [])
            {
                Metadata = new TypeMetadata("Type3"),
            },
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ReplaceIfDirectiveWithElseDeclarationsTest()
    {
        const string code =
            """
            #if D1

            public type Type1 { }

            #else

            public type Type2 { }

            #endif

            public type Type3 { }
            """;
        var tree = Parse(code, []);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        var expected = new SyntaxTree([
            new TypeDeclarationNode(AccessModifier.Public, "Type2", [], [], [], [], [])
            {
                Metadata = new TypeMetadata("Type2"),
            },
            new TypeDeclarationNode(AccessModifier.Public, "Type3", [], [], [], [], [])
            {
                Metadata = new TypeMetadata("Type3"),
            },
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void RemoveIfDirectiveDeclarationsTest()
    {
        const string code =
            """
            #if D1

            public type Type1 { }

            #endif

            public type Type3 { }
            """;
        var tree = Parse(code, []);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        var expected = new SyntaxTree([
            new TypeDeclarationNode(AccessModifier.Public, "Type3", [], [], [], [], [])
            {
                Metadata = new TypeMetadata("Type3"),
            },
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ReplaceIfDirectiveWithThenStatementsTest()
    {
        const string code =
            """
            function test(callback: () => void): i32 {
                callback();

            #if D1
                return 1;
            #else
                return 2;
            #endif
            }
            """;
        var directives = new[] { "D1" };
        var tree = Parse(code, directives);

        var lowering = new Lowering();
        lowering.Lower(tree, new LoweringOptions(directives));

        var expected = new SyntaxTree([
            new FunctionDeclarationNode(
                "test",
                [
                    new ParameterNode(
                        "callback",
                        new FunctionTypeNode([], new TypeNode("void") { Metadata = TypeMetadata.Void })
                        {
                            Metadata = new FunctionTypeMetadata([], TypeMetadata.Void),
                        }
                    )
                ],
                new TypeNode("i32") { Metadata = TypeMetadata.I32 },
                new BlockStatementNode([
                    new ExpressionStatementNode(
                        new CallExpressionNode(
                            new MemberAccessExpressionNode("callback")
                            {
                                ReturnTypeMetadata = new FunctionTypeMetadata([], TypeMetadata.Void),
                            },
                            []
                        )
                    ),
                    new ReturnStatementNode(
                        new LiteralExpressionNode(LiteralExpressionKind.Number, 1)
                        {
                            ReturnTypeMetadata = TypeMetadata.I32,
                        }
                    )
                ])
            )
            {
                Metadata = new FunctionMetadata(
                    "test",
                    [new ParameterMetadata("callback", new FunctionTypeMetadata([], TypeMetadata.Void))],
                    new FunctionTypeMetadata(
                        [new FunctionTypeMetadata([], TypeMetadata.Void)],
                        TypeMetadata.I32
                    )
                )
            }
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ReplaceIfDirectiveWithElseStatementsTest()
    {
        const string code =
            """
            function test(callback: () => void): i32 {
                callback();

            #if D1
                return 1;
            #else
                return 2;
            #endif
            }
            """;
        var tree = Parse(code, []);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        var expected = new SyntaxTree([
            new FunctionDeclarationNode(
                "test",
                [
                    new ParameterNode(
                        "callback",
                        new FunctionTypeNode([], new TypeNode("void") { Metadata = TypeMetadata.Void })
                        {
                            Metadata = new FunctionTypeMetadata([], TypeMetadata.Void)
                        }
                    )
                ],
                new TypeNode("i32") { Metadata = TypeMetadata.I32 },
                new BlockStatementNode([
                    new ExpressionStatementNode(
                        new CallExpressionNode(
                            new MemberAccessExpressionNode("callback")
                            {
                                ReturnTypeMetadata = new FunctionTypeMetadata([], TypeMetadata.Void)
                            },
                            []
                        )
                    ),
                    new ReturnStatementNode(
                        new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
                        {
                            ReturnTypeMetadata = TypeMetadata.I32
                        }
                    )
                ])
            )
            {
                Metadata = new FunctionMetadata(
                    "test",
                    [new ParameterMetadata("callback", new FunctionTypeMetadata([], TypeMetadata.Void))],
                    new FunctionTypeMetadata(
                        [new FunctionTypeMetadata([], TypeMetadata.Void)],
                        TypeMetadata.I32
                    )
                )
            }
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void RemoveIfDirectiveWithStatementsTest()
    {
        const string code =
            """
            function test(callback: () => void): i32 {
                callback();

            #if D1
                return 1;
            #endif

                return 2;
            }
            """;
        var tree = Parse(code, []);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        var expected = new SyntaxTree([
            new FunctionDeclarationNode(
                "test",
                [
                    new ParameterNode(
                        "callback",
                        new FunctionTypeNode([], new TypeNode("void") { Metadata = TypeMetadata.Void })
                        {
                            Metadata = new FunctionTypeMetadata([], TypeMetadata.Void),
                        }
                    )
                ],
                new TypeNode("i32") { Metadata = TypeMetadata.I32 },
                new BlockStatementNode([
                    new ExpressionStatementNode(
                        new CallExpressionNode(
                            new MemberAccessExpressionNode("callback")
                            {
                                ReturnTypeMetadata = new FunctionTypeMetadata([], TypeMetadata.Void),
                            },
                            []
                        )
                    ),
                    new ReturnStatementNode(
                        new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
                        {
                            ReturnTypeMetadata = TypeMetadata.I32,
                        }
                    )
                ])
            )
            {
                Metadata = new FunctionMetadata(
                    "test",
                    [new ParameterMetadata("callback", new FunctionTypeMetadata([], TypeMetadata.Void))],
                    new FunctionTypeMetadata(
                        [new FunctionTypeMetadata([], TypeMetadata.Void)],
                        TypeMetadata.I32
                    )
                )
            }
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }
}