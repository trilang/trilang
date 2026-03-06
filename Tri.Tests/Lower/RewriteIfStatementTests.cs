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

public class RewriteIfStatementTests
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
    public void RewriteIfElseStatementTest()
    {
        var tree = Parse(
            """
            public test(a: i32): i32 {
                if (a >= 0) {
                    return a;
                } else {
                    return -a;
                }
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = NamespaceMetadata.CreateRoot(builtInTypes);
        var parameterMetadata = new ParameterMetadata(null, "a", builtInTypes.I32);
        var expected = new SemanticTree(file, null, null, [], [
            new FunctionDeclaration(
                null,
                AccessModifier.Public,
                "test",
                [
                    new Parameter(null, "a", new TypeRef(null, "i32") { Metadata = builtInTypes.I32 })
                    {
                        Metadata = parameterMetadata,
                    }
                ],
                new TypeRef(null, "i32") { Metadata = builtInTypes.I32 },
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


        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void RewriteIfWithoutElseStatementTest()
    {
        var tree = Parse(
            """
            public test(a: i32): i32 {
                if (a >= 0) {
                    return a;
                }

                return -a;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = NamespaceMetadata.CreateRoot(builtInTypes);
        var parameterMetadata = new ParameterMetadata(null, "a", builtInTypes.I32);
        var expected = new SemanticTree(file, null, null, [], [
            new FunctionDeclaration(
                null,
                AccessModifier.Public,
                "test",
                [
                    new Parameter(null, "a", new TypeRef(null, "i32") { Metadata = builtInTypes.I32 })
                    {
                        Metadata = parameterMetadata,
                    }
                ],
                new TypeRef(null, "i32") { Metadata = builtInTypes.I32 },
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


        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }
}