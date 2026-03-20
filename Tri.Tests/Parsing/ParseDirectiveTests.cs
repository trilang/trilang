using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseDirectiveTests
{
    private static readonly SourceFile file = new SourceFile("test.tri");

    private static (SyntaxTree, DiagnosticCollection) Parse(string code)
    {
        var diagnostics = new DiagnosticCollection();
        var lexer = new Lexer();
        var lexerOptions = new LexerOptions(new LexerDiagnosticReporter(diagnostics, file));
        var tokens = lexer.Tokenize(code, lexerOptions);
        var parser = new Parser();
        var parserOptions = new ParserOptions(file, new ParserDiagnosticReporter(diagnostics, file));
        var tree = parser.Parse(tokens, parserOptions);

        return (tree, diagnostics);
    }

    [Test]
    public void ParseIfDirectiveTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            #if D1

            public type Type1 { }

            #endif
            """);
        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new IfDirectiveNode(
                    default,
                    "D1",
                    [
                        new TypeDeclarationNode(
                            default,
                            AccessModifier.Public,
                            "Type1",
                            [],
                            [],
                            [],
                            [],
                            [])
                    ],
                    []
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseIfDirectiveWithElseTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            #if D1

            public type Type1 { }

            #else

            public type Type2 { }

            #endif
            """);
        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new IfDirectiveNode(
                    default,
                    "D1",
                    [
                        new TypeDeclarationNode(
                            default,
                            AccessModifier.Public,
                            "Type1",
                            [],
                            [],
                            [],
                            [],
                            [])
                    ],
                    [
                        new TypeDeclarationNode(
                            default,
                            AccessModifier.Public,
                            "Type2",
                            [],
                            [],
                            [],
                            [],
                            [])
                    ]
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseNestedIfDirectiveTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            #if D1

            public type Type1 { }

            #if D2

            public type Type2 { }

            #endif

            public type Type3 { }

            #endif
            """);
        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new IfDirectiveNode(
                    default,
                    "D1",
                    [
                        new TypeDeclarationNode(
                            default,
                            AccessModifier.Public,
                            "Type1",
                            [],
                            [],
                            [],
                            [],
                            []),
                        new IfDirectiveNode(
                            default,
                            "D2",
                            [
                                new TypeDeclarationNode(
                                    default,
                                    AccessModifier.Public,
                                    "Type2",
                                    [],
                                    [],
                                    [],
                                    [],
                                    [])
                            ],
                            []),
                        new TypeDeclarationNode(
                            default,
                            AccessModifier.Public,
                            "Type3",
                            [],
                            [],
                            [],
                            [],
                            []),
                    ],
                    []
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseIfDirectiveStatementTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
            #if D1
                print("D1");
            #else
                print("Empty");
            #endif
            }
            """);
        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                FunctionDeclarationNode.Create(
                    default,
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(default, ["void"]),
                    new BlockStatementNode(
                        default,
                        [
                            new IfDirectiveNode(
                                default,
                                "D1",
                                [
                                    new ExpressionStatementNode(
                                        default,
                                        new CallExpressionNode(
                                            default,
                                            new MemberAccessExpressionNode(
                                                default,
                                                "print"
                                            ),
                                            [
                                                LiteralExpressionNode.String(
                                                    default,
                                                    "D1")
                                            ]
                                        )
                                    )
                                ],
                                [
                                    new ExpressionStatementNode(
                                        default,
                                        new CallExpressionNode(
                                            default,
                                            new MemberAccessExpressionNode(
                                                default,
                                                "print"
                                            ),
                                            [
                                                LiteralExpressionNode.String(
                                                    default,
                                                    "Empty")
                                            ]
                                        )
                                    )
                                ]
                            )
                        ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseEmptyIfDirectiveTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            #if D1
            #else
            #endif
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new IfDirectiveNode(
                    default,
                    "D1",
                    [],
                    []
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }
}