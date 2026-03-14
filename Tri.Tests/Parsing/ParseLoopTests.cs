using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseLoopTests
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
    public void ParseWhileTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(x: i32): void {
                while (x > 0) {
                }
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
                    "test",
                    [
                        new ParameterNode(
                            default,
                            "x",
                            new TypeRefNode(
                                default,
                                "i32"
                            )
                        )
                    ],
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(
                        default,
                        [
                            new WhileNode(
                                default,
                                new BinaryExpressionNode(
                                    default,
                                    BinaryExpressionKind.GreaterThan,
                                    new MemberAccessExpressionNode(default, "x"),
                                    LiteralExpressionNode.Integer(default, 0)
                                ),
                                new BlockStatementNode(default)
                            )
                        ]
                    )
                ),
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseWhileMissingOpenParenTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(x: i32): void {
                while x > 0) {
                }
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
                    "test",
                    [
                        new ParameterNode(
                            default,
                            "x",
                            new TypeRefNode(
                                default,
                                "i32"
                            )
                        )
                    ],
                    new TypeRefNode(
                        default,
                        "void"
                    ),
                    new BlockStatementNode(
                        default,
                        [
                            new WhileNode(
                                default,
                                new BinaryExpressionNode(
                                    default,
                                    BinaryExpressionKind.GreaterThan,
                                    new MemberAccessExpressionNode(
                                        default,
                                        "x"
                                    ),
                                    LiteralExpressionNode.Integer(
                                        default,
                                        0
                                    )
                                ),
                                new BlockStatementNode(default)
                            )
                        ]
                    )
                ),
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(56, 4, 11).ToSpan()),
            "Expected '('.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseWhileMissingConditionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(x: i32): void {
                while (;) {
                }
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
                    "test",
                    [
                        new ParameterNode(
                            default,
                            "x",
                            new TypeRefNode(
                                default,
                                "i32"
                            )
                        )
                    ],
                    new TypeRefNode(
                        default,
                        "void"
                    ),
                    new BlockStatementNode(
                        default,
                        [
                            new WhileNode(
                                default,
                                new FakeExpressionNode(default),
                                new BlockStatementNode(
                                    new SourceSpan(new SourcePosition(42, 2, 15), new SourcePosition(49, 3, 6))
                                )
                            )
                        ]
                    )
                ),
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0009ExpectedExpression,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourceSpan(new SourcePosition(57, 4, 12), new SourcePosition(58, 4, 13))),
            "Expected an expression.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseWhileMissingCloseParenTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(x: i32): void {
                while (x > 0 {
                }
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
                    "test",
                    [
                        new ParameterNode(
                            default,
                            "x",
                            new TypeRefNode(
                                default,
                                "i32"
                            )
                        )
                    ],
                    new TypeRefNode(
                        default,
                        "void"
                    ),
                    new BlockStatementNode(
                        default,
                        [
                            new WhileNode(
                                default,
                                new BinaryExpressionNode(
                                    default,
                                    BinaryExpressionKind.GreaterThan,
                                    new MemberAccessExpressionNode(
                                        default,
                                        "x"
                                    ),
                                    LiteralExpressionNode.Integer(
                                        default,
                                        0
                                    )
                                ),
                                new BlockStatementNode(default)
                            )
                        ]
                    )
                ),
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(63, 4, 18).ToSpan()),
            "Expected ')'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseWhileMissingBlockTest()
    {
        var source = """
                     namespace Test1;

                     public test(x: i32): void {
                         while (x > 0)
                            ;
                     }
                     """;

        var (tree, diagnostics) = Parse(source);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                FunctionDeclarationNode.Create(
                    default,
                    AccessModifier.Public,
                    "test",
                    [
                        new ParameterNode(
                            default,
                            "x",
                            new TypeRefNode(
                                default,
                                "i32"
                            )
                        )
                    ],
                    new TypeRefNode(
                        default,
                        "void"
                    ),
                    new BlockStatementNode(
                        default,
                        [
                            new WhileNode(
                                default,
                                new BinaryExpressionNode(
                                    default,
                                    BinaryExpressionKind.GreaterThan,
                                    new MemberAccessExpressionNode(
                                        default,
                                        "x"
                                    ),
                                    LiteralExpressionNode.Integer(
                                        default,
                                        0
                                    )
                                ),
                                new BlockStatementNode(
                                    default,
                                    [
                                        new FakeStatementNode(
                                            new SourcePosition(53, 3, 8).ToSpan()
                                        )
                                    ]
                                )
                            )
                        ]
                    )
                ),
            ]);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(71, 5, 8).ToSpan()),
                "Expected '{'."),
            new Diagnostic(
                DiagnosticId.P0004ExpectedStatement,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(71, 5, 8).ToSpan()),
                "Expected a statement."),
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(74, 6, 2).ToSpan()),
                "Expected '}'."),
        };

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseBreakTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(x: i32): void {
                while (x > 0) {
                    break;
                }
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
                    "test",
                    [
                        new ParameterNode(
                            default,
                            "x",
                            new TypeRefNode(
                                default,
                                "i32"
                            )
                        )
                    ],
                    new TypeRefNode(
                        default,
                        "void"
                    ),
                    new BlockStatementNode(
                        default,
                        [
                            new WhileNode(
                                default,
                                new BinaryExpressionNode(
                                    default,
                                    BinaryExpressionKind.GreaterThan,
                                    new MemberAccessExpressionNode(
                                        default,
                                        "x"
                                    ),
                                    LiteralExpressionNode.Integer(
                                        default,
                                        0
                                    )
                                ),
                                new BlockStatementNode(
                                    default,
                                    [
                                        new BreakNode(
                                            new SourceSpan(new SourcePosition(56, 3, 9), new SourcePosition(62, 3, 15))
                                        )
                                    ]
                                )
                            )
                        ])
                ),
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseContinueTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(x: i32): void {
                while (x > 0) {
                    continue;
                }
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
                    "test",
                    [
                        new ParameterNode(
                            default,
                            "x",
                            new TypeRefNode(
                                default,
                                "i32"
                            )
                        )
                    ],
                    new TypeRefNode(
                        default,
                        "void"
                    ),
                    new BlockStatementNode(
                        default,
                        [
                            new WhileNode(
                                default,
                                new BinaryExpressionNode(
                                    default,
                                    BinaryExpressionKind.GreaterThan,
                                    new MemberAccessExpressionNode(
                                        default,
                                        "x"
                                    ),
                                    LiteralExpressionNode.Integer(
                                        default,
                                        0
                                    )
                                ),
                                new BlockStatementNode(
                                    default,
                                    [
                                        new ContinueNode(default)
                                    ]
                                )
                            )
                        ])
                ),
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }
}