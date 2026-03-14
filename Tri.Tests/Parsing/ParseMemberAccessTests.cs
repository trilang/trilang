using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseMemberAccessTests
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
    public void ParseArrayAccessTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(x: i32[]): void {
                var a: i32 = x[0];
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
                    [new ParameterNode(default, "x", new ArrayTypeNode(default, new TypeRefNode(default, "i32")))],
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(default, [
                        new VariableDeclarationNode(
                            default,
                            "a",
                            new TypeRefNode(default, "i32"),
                            new ArrayAccessExpressionNode(
                                default,
                                new MemberAccessExpressionNode(default, "x"),
                                LiteralExpressionNode.Integer(default, 0)
                            )
                        )
                    ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseArrayAccessMissingCloseTest()
    {
        const string code =
            """
            namespace Test1;

            public test(x: i32[]): void {
                var a: i32 = x[0;
            }
            """;
        var (tree, diagnostics) = Parse(code);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new FunctionDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "test",
                    [
                        new ParameterNode(
                            default,
                            "x",
                            new ArrayTypeNode(
                                default,
                                new TypeRefNode(
                                    default,
                                    "i32"
                                )
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
                            new VariableDeclarationNode(
                                default,
                                "a",
                                new TypeRefNode(
                                    default,
                                    "i32"
                                ),
                                new ArrayAccessExpressionNode(
                                    default,
                                    new MemberAccessExpressionNode(
                                        default,
                                        "x"
                                    ),
                                    LiteralExpressionNode.Integer(
                                        default,
                                        0
                                    )
                                )
                            )
                        ]
                    )
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(68, 4, 21).ToSpan()),
            "Expected ']'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseSetArrayTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(): void {
                x[0] = 1;
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
                    [],
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(default, [
                        new ExpressionStatementNode(
                            default,
                            new BinaryExpressionNode(
                                default,
                                BinaryExpressionKind.Assignment,
                                new ArrayAccessExpressionNode(
                                    default,
                                    new MemberAccessExpressionNode(default, "x"),
                                    LiteralExpressionNode.Integer(default, 0)
                                ),
                                LiteralExpressionNode.Integer(default, 1)
                            )
                        )
                    ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseSetNestedArrayTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(): void {
                a.b[0].c = 1;
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
                    [],
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(default, [
                        new ExpressionStatementNode(
                            default,
                            new BinaryExpressionNode(
                                default,
                                BinaryExpressionKind.Assignment,
                                new MemberAccessExpressionNode(
                                    default,
                                    new ArrayAccessExpressionNode(
                                        default,
                                        new MemberAccessExpressionNode(
                                            default,
                                            new MemberAccessExpressionNode(default, "a"),
                                            "b"
                                        ),
                                        LiteralExpressionNode.Integer(default, 0)
                                    ),
                                    "c"
                                ),
                                LiteralExpressionNode.Integer(default, 1)
                            )
                        )
                    ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseMultipleArrayAccessTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                return a[0][1];
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
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(default, [
                        new ReturnStatementNode(
                            default,
                            new ArrayAccessExpressionNode(
                                default,
                                new ArrayAccessExpressionNode(
                                    default,
                                    new MemberAccessExpressionNode(default, "a"),
                                    LiteralExpressionNode.Integer(default, 0)
                                ),
                                LiteralExpressionNode.Integer(default, 1)
                            )
                        )
                    ]))
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseMultipleMemberAccessTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                return a.b.c;
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
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(default, [
                        new ReturnStatementNode(
                            default,
                            new MemberAccessExpressionNode(
                                default,
                                new MemberAccessExpressionNode(
                                    default,
                                    new MemberAccessExpressionNode(default, "a"),
                                    "b"
                                ),
                                "c"
                            )
                        )
                    ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseMultipleMemberAccessMissingExpressionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                return a.b.;
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new FunctionDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(
                        default,
                        "void"
                    ),
                    new BlockStatementNode(
                        default,
                        [
                            new ReturnStatementNode(
                                default,
                                new FakeExpressionNode(default)
                            ),
                        ]
                    )
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0013ExpectedIdentifier,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourceSpan(new SourcePosition(55, 4, 16), new SourcePosition(56, 4, 17))),
            "Expected an identifier.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMemberAccessNestedCallTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                return a.b().c;
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
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(default, [
                        new ReturnStatementNode(
                            default,
                            new MemberAccessExpressionNode(
                                default,
                                new CallExpressionNode(
                                    default,
                                    new MemberAccessExpressionNode(
                                        default,
                                        new MemberAccessExpressionNode(default, "a"),
                                        "b"
                                    ),
                                    []
                                ),
                                "c"
                            )
                        )
                    ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseMultipleCallsTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                f(1)(2);
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
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(default, [
                        new ExpressionStatementNode(
                            default,
                            new CallExpressionNode(
                                default,
                                new CallExpressionNode(
                                    default,
                                    new MemberAccessExpressionNode(default, "f"),
                                    [LiteralExpressionNode.Integer(default, 1)]
                                ),
                                [LiteralExpressionNode.Integer(default, 2)]
                            )
                        )
                    ]))
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseMemberAccessAfterCtorTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                return new Test().a;
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
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(default, [
                        new ReturnStatementNode(
                            default,
                            new MemberAccessExpressionNode(
                                default,
                                new NewObjectExpressionNode(
                                    default,
                                    new TypeRefNode(default, "Test"),
                                    []
                                ),
                                "a"
                            )
                        )
                    ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseMemberAccessAfterNewArrayTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                return new i32[0].size;
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
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(default, [
                        new ReturnStatementNode(
                            default,
                            new MemberAccessExpressionNode(
                                default,
                                new NewArrayExpressionNode(
                                    default,
                                    new ArrayTypeNode(default, new TypeRefNode(default, "i32")),
                                    LiteralExpressionNode.Integer(default, 0)
                                ),
                                "size"
                            )
                        )
                    ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseCallExpWithBinaryExpTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                return 1.toString();
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
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(default, [
                        new ReturnStatementNode(
                            default,
                            new CallExpressionNode(
                                default,
                                new MemberAccessExpressionNode(
                                    default,
                                    LiteralExpressionNode.Integer(default, 1),
                                    "toString"
                                ),
                                []
                            )
                        )
                    ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseCallExpWithParenExpTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                return 1 + 2.toString();
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
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(default, [
                        new ReturnStatementNode(
                            default,
                            new BinaryExpressionNode(
                                default,
                                BinaryExpressionKind.Addition,
                                LiteralExpressionNode.Integer(default, 1),
                                new CallExpressionNode(
                                    default,
                                    new MemberAccessExpressionNode(
                                        default,
                                        LiteralExpressionNode.Integer(default, 2),
                                        "toString"
                                    ),
                                    []
                                )
                            )
                        )
                    ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseTupleMemberAccessTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(t: (i32, string)): i32 {
                return t.0;
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
                            "t",
                            new TupleTypeNode(default, [
                                new TypeRefNode(default, "i32"),
                                new TypeRefNode(default, "string")
                            ])
                        )
                    ],
                    new TypeRefNode(default, "i32"),
                    new BlockStatementNode(default, [
                        new ReturnStatementNode(
                            default,
                            new MemberAccessExpressionNode(
                                default,
                                new MemberAccessExpressionNode(default, "t"),
                                "0"
                            )
                        )
                    ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseTupleMemberAccessWithIncorrectIndexTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(t: (i32, string)): i32 {
                return t.0x;
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new FunctionDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "test",
                    [
                        new ParameterNode(
                            default,
                            "t",
                            new TupleTypeNode(
                                default,
                                [
                                    new TypeRefNode(
                                        default,
                                        "i32"
                                    ),
                                    new TypeRefNode(
                                        default,
                                        "string"
                                    ),
                                ]
                            )
                        )
                    ],
                    new TypeRefNode(
                        default,
                        "i32"
                    ),
                    new BlockStatementNode(
                        default,
                        [
                            new ReturnStatementNode(
                                default,
                                new MemberAccessExpressionNode(
                                    default,
                                    new MemberAccessExpressionNode(
                                        default,
                                        "t"
                                    ),
                                    "0"
                                )
                            ),
                            new ExpressionStatementNode(
                                default,
                                new MemberAccessExpressionNode(
                                    default,
                                    "x"
                                )
                            )
                        ]
                    )
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(69, 4, 15).ToSpan()),
            "Expected ';'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }
}