using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Lower;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using Trilang.Semantics.Passes.ControlFlow;

namespace Tri.Tests.Lower;

public class ReplaceIfDirectivesTests
{
    private static readonly SourceFile file = new SourceFile("test.tri");

    private static SemanticTree Parse(string code, IEnumerable<string> directives)
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
            new SemanticAnalysisOptions(directives, new SemanticDiagnosticReporter(diagnostics)));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        return semanticTrees.Single();
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

        var type1Metadata = new TypeMetadata(null, "Type1");
        type1Metadata.AddConstructor(
            new ConstructorMetadata(
                null,
                type1Metadata,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata(null, [], type1Metadata)));

        var type3Metadata = new TypeMetadata(null, "Type3");
        type3Metadata.AddConstructor(
            new ConstructorMetadata(
                null,
                type3Metadata,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata(null, [], type3Metadata)));

        var expected = new SemanticTree(file, null, [], null, [
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

        var type2Metadata = new TypeMetadata(null, "Type2");
        type2Metadata.AddConstructor(
            new ConstructorMetadata(
                null,
                type2Metadata,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata(null, [], type2Metadata)));

        var type3Metadata = new TypeMetadata(null, "Type3");
        type3Metadata.AddConstructor(
            new ConstructorMetadata(
                null,
                type3Metadata,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata(null, [], type3Metadata)));

        var expected = new SemanticTree(file, null, [], null, [
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

        var typeMetadata = new TypeMetadata(null, "Type3");
        typeMetadata.AddConstructor(
            new ConstructorMetadata(
                null,
                typeMetadata,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata(null, [], typeMetadata)));

        var expected = new SemanticTree(file, null, [], null, [
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
            public test(callback: () => void): i32 {
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
            null,
            "callback",
            new FunctionTypeMetadata(null, [], TypeMetadata.Void)
        );
        var expected = new SemanticTree(file, null, [], null, [
            new FunctionDeclaration(
                null,
                AccessModifier.Public,
                "test",
                [
                    new Parameter(
                        null,
                        "callback",
                        new FunctionType(null, [], new TypeRef(null, "void") { Metadata = TypeMetadata.Void })
                        {
                            Metadata = new FunctionTypeMetadata(null, [], TypeMetadata.Void),
                        }
                    )
                    {
                        Metadata = parameterMetadata,
                    }
                ],
                new TypeRef(null, "i32") { Metadata = TypeMetadata.I32 },
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
                    null,
                    AccessModifierMetadata.Public,
                    "test",
                    [parameterMetadata],
                    new FunctionTypeMetadata(
                        null,
                        [new FunctionTypeMetadata(null, [], TypeMetadata.Void)],
                        TypeMetadata.I32
                    ),
                    new FunctionGroupMetadata()
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
            public test(callback: () => void): i32 {
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
            null,
            "callback",
            new FunctionTypeMetadata(null, [], TypeMetadata.Void)
        );
        var expected = new SemanticTree(file, null, [], null, [
            new FunctionDeclaration(
                null,
                AccessModifier.Public,
                "test",
                [
                    new Parameter(
                        null,
                        "callback",
                        new FunctionType(null, [], new TypeRef(null, "void") { Metadata = TypeMetadata.Void })
                        {
                            Metadata = new FunctionTypeMetadata(null, [], TypeMetadata.Void)
                        }
                    )
                    {
                        Metadata = parameterMetadata,
                    }
                ],
                new TypeRef(null, "i32") { Metadata = TypeMetadata.I32 },
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
                    null,
                    AccessModifierMetadata.Public,
                    "test",
                    [parameterMetadata],
                    new FunctionTypeMetadata(
                        null,
                        [new FunctionTypeMetadata(null, [], TypeMetadata.Void)],
                        TypeMetadata.I32
                    ),
                    new FunctionGroupMetadata()
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
            public test(callback: () => void): i32 {
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
            null,
            "callback",
            new FunctionTypeMetadata(null, [], TypeMetadata.Void)
        );
        var expected = new SemanticTree(file, null, [], null, [
            new FunctionDeclaration(
                null,
                AccessModifier.Public,
                "test",
                [
                    new Parameter(
                        null,
                        "callback",
                        new FunctionType(null, [], new TypeRef(null, "void") { Metadata = TypeMetadata.Void })
                        {
                            Metadata = new FunctionTypeMetadata(null, [], TypeMetadata.Void),
                        }
                    )
                    {
                        Metadata = parameterMetadata,
                    }
                ],
                new TypeRef(null, "i32") { Metadata = TypeMetadata.I32 },
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
                    null,
                    AccessModifierMetadata.Public,
                    "test",
                    [parameterMetadata],
                    new FunctionTypeMetadata(
                        null,
                        [new FunctionTypeMetadata(null, [], TypeMetadata.Void)],
                        TypeMetadata.I32
                    ),
                    new FunctionGroupMetadata()
                )
            }
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }
}