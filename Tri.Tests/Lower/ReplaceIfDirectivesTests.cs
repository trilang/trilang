using Trilang.Lower;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using Trilang.Semantics.Passes.ControlFlow;
using Type = Trilang.Semantics.Model.Type;

namespace Tri.Tests.Lower;

public class ReplaceIfDirectivesTests
{
    private static SemanticTree Parse(string code, IEnumerable<string> directives)
    {
        var parser = new Parser();
        var tree = parser.Parse(code);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, new SemanticAnalysisOptions(directives));

        return semanticTree;
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
        lowering.Lower(tree, new LoweringOptions(directives, new ControlFlowGraphMap()));

        var type1Metadata = new TypeMetadata("Type1");
        type1Metadata.AddConstructor(
            new ConstructorMetadata(
                type1Metadata,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata([], type1Metadata)));

        var type3Metadata = new TypeMetadata("Type3");
        type3Metadata.AddConstructor(
            new ConstructorMetadata(
                type3Metadata,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata([], type3Metadata)));

        var expected = new SemanticTree(null, [
            new TypeDeclaration(null, AccessModifier.Public, "Type1", [], [], [], [], [])
            {
                Metadata = type1Metadata,
            },
            new TypeDeclaration(null, AccessModifier.Public, "Type3", [], [], [], [], [])
            {
                Metadata = type3Metadata,
            },
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
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

        var type2Metadata = new TypeMetadata("Type2");
        type2Metadata.AddConstructor(
            new ConstructorMetadata(
                type2Metadata,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata([], type2Metadata)));

        var type3Metadata = new TypeMetadata("Type3");
        type3Metadata.AddConstructor(
            new ConstructorMetadata(
                type3Metadata,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata([], type3Metadata)));

        var expected = new SemanticTree(null, [
            new TypeDeclaration(null, AccessModifier.Public, "Type2", [], [], [], [], [])
            {
                Metadata = type2Metadata,
            },
            new TypeDeclaration(null, AccessModifier.Public, "Type3", [], [], [], [], [])
            {
                Metadata = type3Metadata,
            },
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
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

        var typeMetadata = new TypeMetadata("Type3");
        typeMetadata.AddConstructor(
            new ConstructorMetadata(
                typeMetadata,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata([], typeMetadata)));

        var expected = new SemanticTree(null, [
            new TypeDeclaration(null, AccessModifier.Public, "Type3", [], [], [], [], [])
            {
                Metadata = typeMetadata,
            },
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
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
        lowering.Lower(tree, new LoweringOptions(directives, new ControlFlowGraphMap()));

        var parameterMetadata = new ParameterMetadata(
            "callback",
            new FunctionTypeMetadata([], TypeMetadata.Void)
        );
        var expected = new SemanticTree(null, [
            new FunctionDeclaration(
                null,
                "test",
                [
                    new Parameter(
                        null,
                        "callback",
                        new FunctionType(null, [], new Type(null, "void") { Metadata = TypeMetadata.Void })
                        {
                            Metadata = new FunctionTypeMetadata([], TypeMetadata.Void),
                        }
                    )
                    {
                        Metadata = parameterMetadata,
                    }
                ],
                new Type(null, "i32") { Metadata = TypeMetadata.I32 },
                new BlockStatement(null, [
                    new ExpressionStatement(
                        null,
                        new CallExpression(
                            null,
                            new MemberAccessExpression(null, "callback")
                            {
                                Reference = parameterMetadata,
                                AccessKind = MemberAccessKind.Read,
                            },
                            []
                        )
                    ),
                    new ReturnStatement(
                        null,
                        new LiteralExpression(null, LiteralExpressionKind.Integer, 1)
                        {
                            ReturnTypeMetadata = TypeMetadata.I32,
                        }
                    )
                ])
            )
            {
                Metadata = new FunctionMetadata(
                    "test",
                    [parameterMetadata],
                    new FunctionTypeMetadata(
                        [new FunctionTypeMetadata([], TypeMetadata.Void)],
                        TypeMetadata.I32
                    )
                )
            }
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
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

        var parameterMetadata = new ParameterMetadata(
            "callback",
            new FunctionTypeMetadata([], TypeMetadata.Void)
        );
        var expected = new SemanticTree(null, [
            new FunctionDeclaration(
                null,
                "test",
                [
                    new Parameter(
                        null,
                        "callback",
                        new FunctionType(null, [], new Type(null, "void") { Metadata = TypeMetadata.Void })
                        {
                            Metadata = new FunctionTypeMetadata([], TypeMetadata.Void)
                        }
                    )
                    {
                        Metadata = parameterMetadata,
                    }
                ],
                new Type(null, "i32") { Metadata = TypeMetadata.I32 },
                new BlockStatement(null, [
                    new ExpressionStatement(
                        null,
                        new CallExpression(
                            null,
                            new MemberAccessExpression(null, "callback")
                            {
                                Reference = parameterMetadata,
                                AccessKind = MemberAccessKind.Read,
                            },
                            []
                        )
                    ),
                    new ReturnStatement(
                        null,
                        new LiteralExpression(null, LiteralExpressionKind.Integer, 2)
                        {
                            ReturnTypeMetadata = TypeMetadata.I32
                        }
                    )
                ])
            )
            {
                Metadata = new FunctionMetadata(
                    "test",
                    [parameterMetadata],
                    new FunctionTypeMetadata(
                        [new FunctionTypeMetadata([], TypeMetadata.Void)],
                        TypeMetadata.I32
                    )
                )
            }
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
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

        var parameterMetadata = new ParameterMetadata(
            "callback",
            new FunctionTypeMetadata([], TypeMetadata.Void)
        );
        var expected = new SemanticTree(null, [
            new FunctionDeclaration(
                null,
                "test",
                [
                    new Parameter(
                        null,
                        "callback",
                        new FunctionType(null, [], new Type(null, "void") { Metadata = TypeMetadata.Void })
                        {
                            Metadata = new FunctionTypeMetadata([], TypeMetadata.Void),
                        }
                    )
                    {
                        Metadata = parameterMetadata,
                    }
                ],
                new Type(null, "i32") { Metadata = TypeMetadata.I32 },
                new BlockStatement(null, [
                    new ExpressionStatement(
                        null,
                        new CallExpression(
                            null,
                            new MemberAccessExpression(null, "callback")
                            {
                                Reference = parameterMetadata,
                                AccessKind = MemberAccessKind.Read,
                            },
                            []
                        )
                    ),
                    new ReturnStatement(
                        null,
                        new LiteralExpression(null, LiteralExpressionKind.Integer, 2)
                        {
                            ReturnTypeMetadata = TypeMetadata.I32,
                        }
                    )
                ])
            )
            {
                Metadata = new FunctionMetadata(
                    "test",
                    [parameterMetadata],
                    new FunctionTypeMetadata(
                        [new FunctionTypeMetadata([], TypeMetadata.Void)],
                        TypeMetadata.I32
                    )
                )
            }
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }
}