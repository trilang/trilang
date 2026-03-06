using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Lower;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using Trilang.Semantics.Passes.ControlFlow;
using static Tri.Tests.Factory;

namespace Tri.Tests.Lower;

public class ReplaceIfDirectivesTests
{
    private static readonly SourceFile file = new SourceFile("test.tri");

    private static SemanticTree Parse(string code, IReadOnlyList<string> directives)
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
            new SemanticAnalysisOptions(
                directives.ToHashSet(),
                new SemanticDiagnosticReporter(diagnostics),
                builtInTypes));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var semanticTree = semanticTrees.Single();

        var lowering = new Lowering(builtInTypes);
        lowering.Lower(semanticTree, new LoweringOptions(directives.ToHashSet(), new ControlFlowGraphMap()));

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

        var tree = Parse(code, ["D1"]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = NamespaceMetadata.CreateRoot(builtInTypes);
        var type1Metadata = new TypeMetadata(null, "Type1")
        {
            Namespace = rootNamespace,
        };
        type1Metadata.AddConstructor(
            new ConstructorMetadata(
                null,
                type1Metadata,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, rootNamespace)));

        var type3Metadata = new TypeMetadata(null, "Type3")
        {
            Namespace = rootNamespace,
        };
        type3Metadata.AddConstructor(
            new ConstructorMetadata(
                null,
                type3Metadata,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, rootNamespace)));

        var expected = new SemanticTree(file, null, null, [], [
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

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = NamespaceMetadata.CreateRoot(builtInTypes);
        var type2Metadata = new TypeMetadata(null, "Type2")
        {
            Namespace = rootNamespace,
        };
        type2Metadata.AddConstructor(
            new ConstructorMetadata(
                null,
                type2Metadata,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, rootNamespace)));

        var type3Metadata = new TypeMetadata(null, "Type3")
        {
            Namespace = rootNamespace,
        };
        type3Metadata.AddConstructor(
            new ConstructorMetadata(
                null,
                type3Metadata,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, rootNamespace)));

        var expected = new SemanticTree(file, null, null, [], [
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

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = NamespaceMetadata.CreateRoot(builtInTypes);
        var typeMetadata = new TypeMetadata(null, "Type3")
        {
            Namespace = rootNamespace,
        };
        typeMetadata.AddConstructor(
            new ConstructorMetadata(
                null,
                typeMetadata,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], builtInTypes.Void, rootNamespace)));

        var expected = new SemanticTree(file, null, null, [], [
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
        var tree = Parse(code, ["D1"]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = NamespaceMetadata.CreateRoot(builtInTypes);
        var parameterMetadata = new ParameterMetadata(
            null,
            "callback",
            CreateFunctionType([], builtInTypes.Void, rootNamespace));
        var expected = new SemanticTree(file, null, null, [], [
            new FunctionDeclaration(
                null,
                AccessModifier.Public,
                "test",
                [
                    new Parameter(
                        null,
                        "callback",
                        new FunctionType(null, [], new TypeRef(null, "void") { Metadata = builtInTypes.Void })
                        {
                            Metadata = CreateFunctionType([], builtInTypes.Void, rootNamespace),
                        }
                    )
                    {
                        Metadata = parameterMetadata,
                    }
                ],
                new TypeRef(null, "i32") { Metadata = builtInTypes.I32 },
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
                            ReturnTypeMetadata = builtInTypes.I32,
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
                    CreateFunctionType([
                            CreateFunctionType([], builtInTypes.Void, rootNamespace)
                        ],
                        builtInTypes.I32,
                        rootNamespace))
                {
                    Namespace = rootNamespace,
                }
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

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = NamespaceMetadata.CreateRoot(builtInTypes);
        var parameterMetadata = new ParameterMetadata(
            null,
            "callback",
            CreateFunctionType([], builtInTypes.Void, rootNamespace));
        var expected = new SemanticTree(file, null, null, [], [
            new FunctionDeclaration(
                null,
                AccessModifier.Public,
                "test",
                [
                    new Parameter(
                        null,
                        "callback",
                        new FunctionType(null, [], new TypeRef(null, "void") { Metadata = builtInTypes.Void })
                        {
                            Metadata = CreateFunctionType([],
                                builtInTypes.Void,
                                rootNamespace)
                        }
                    )
                    {
                        Metadata = parameterMetadata,
                    }
                ],
                new TypeRef(null, "i32") { Metadata = builtInTypes.I32 },
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
                            ReturnTypeMetadata = builtInTypes.I32
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
                    CreateFunctionType([
                            CreateFunctionType([], builtInTypes.Void, rootNamespace)
                        ],
                        builtInTypes.I32,
                        rootNamespace))
                {
                    Namespace = rootNamespace,
                }
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

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = NamespaceMetadata.CreateRoot(builtInTypes);
        var parameterMetadata = new ParameterMetadata(
            null,
            "callback",
            CreateFunctionType([], builtInTypes.Void, rootNamespace));
        var expected = new SemanticTree(file, null, null, [], [
            new FunctionDeclaration(
                null,
                AccessModifier.Public,
                "test",
                [
                    new Parameter(
                        null,
                        "callback",
                        new FunctionType(null, [], new TypeRef(null, "void") { Metadata = builtInTypes.Void })
                        {
                            Metadata = CreateFunctionType([], builtInTypes.Void, rootNamespace),
                        }
                    )
                    {
                        Metadata = parameterMetadata,
                    }
                ],
                new TypeRef(null, "i32") { Metadata = builtInTypes.I32 },
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
                            ReturnTypeMetadata = builtInTypes.I32,
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
                    CreateFunctionType([
                            CreateFunctionType([], builtInTypes.Void, rootNamespace)
                        ],
                        builtInTypes.I32,
                        rootNamespace))
                {
                    Namespace = rootNamespace,
                }
            }
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }
}