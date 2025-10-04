using Trilang;
using Trilang.Compilation;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseMemberAccessTests
{
    private static readonly SourceFile file = new SourceFile("test.tri", "test.tri");

    private static (SyntaxTree, DiagnosticCollection) Parse(string code)
    {
        var diagnostics = new DiagnosticCollection();
        diagnostics.SwitchFile(file);

        var lexer = new Lexer();
        var tokens = lexer.Tokenize(code, new LexerOptions(diagnostics.Lexer));
        var parser = new Parser();
        var tree = parser.Parse(tokens, new ParserOptions(diagnostics.Parser));

        return (tree, diagnostics);
    }

    [Test]
    public void ParseArrayAccessTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(x: i32[]): void {
                var a: i32 = x[0];
            }
            """);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(54, 3, 2)),
                AccessModifier.Public,
                "test",
                [new ParameterNode(new SourceSpan(new SourcePosition(12, 1, 13), new SourcePosition(20, 1, 21)), "x", new ArrayTypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(20, 1, 21)), new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(18, 1, 19)), "i32")))],
                new TypeNode(new SourceSpan(new SourcePosition(23, 1, 24), new SourcePosition(27, 1, 28)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(28, 1, 29), new SourcePosition(54, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(34, 2, 5), new SourcePosition(52, 2, 23)),
                        "a",
                        new TypeNode(new SourceSpan(new SourcePosition(41, 2, 12), new SourcePosition(44, 2, 15)), "i32"),
                        new ArrayAccessExpressionNode(
                            new SourceSpan(new SourcePosition(47, 2, 18), new SourcePosition(51, 2, 22)),
                            new MemberAccessExpressionNode(new SourceSpan(new SourcePosition(47, 2, 18), new SourcePosition(48, 2, 19)), "x"),
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(49, 2, 20), new SourcePosition(50, 2, 21)), 0)
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
            public test(x: i32[]): void {
                var a: i32 = x[0;
            }
            """;
        var (tree, diagnostics) = Parse(code);

        var expected = new SyntaxTree([
            new FunctionDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(53, 3, 2)),
                AccessModifier.Public,
                "test",
                [
                    new ParameterNode(
                        new SourceSpan(new SourcePosition(12, 1, 13), new SourcePosition(20, 1, 21)),
                        "x",
                        new ArrayTypeNode(
                            new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(20, 1, 21)),
                            new TypeNode(
                                new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(18, 1, 19)),
                                "i32"
                            )
                        )
                    )
                ],
                new TypeNode(
                    new SourceSpan(new SourcePosition(23, 1, 24), new SourcePosition(27, 1, 28)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(28, 1, 29), new SourcePosition(53, 3, 2)),
                    [
                        new VariableDeclarationNode(
                            new SourceSpan(new SourcePosition(34, 2, 5), new SourcePosition(51, 2, 22)),
                            "a",
                            new TypeNode(
                                new SourceSpan(new SourcePosition(41, 2, 12), new SourcePosition(44, 2, 15)),
                                "i32"
                            ),
                            new ArrayAccessExpressionNode(
                                new SourceSpan(new SourcePosition(47, 2, 18), new SourcePosition(50, 2, 21)),
                                new MemberAccessExpressionNode(
                                    new SourceSpan(new SourcePosition(47, 2, 18), new SourcePosition(48, 2, 19)),
                                    "x"
                                ),
                                LiteralExpressionNode.Integer(
                                    new SourceSpan(new SourcePosition(49, 2, 20), new SourcePosition(50, 2, 21)),
                                    0
                                )
                            )
                        )
                    ]
                )
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticIds.P0001_MissingToken,
            DiagnosticSeverity.Error,
            file,
            new SourcePosition(50, 2, 21).ToSpan(),
            "Expected ']'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseSetArrayTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(): void {
                x[0] = 1;
            }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(37, 3, 2)),
                AccessModifier.Public,
                "test",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(37, 3, 2)), [
                    new ExpressionStatementNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(35, 2, 14)),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(34, 2, 13)),
                            BinaryExpressionKind.Assignment,
                            new ArrayAccessExpressionNode(
                                new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(30, 2, 9)),
                                new MemberAccessExpressionNode(new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(27, 2, 6)), "x"),
                                LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(28, 2, 7), new SourcePosition(29, 2, 8)), 0)
                            ),
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(34, 2, 13)), 1)
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
            public test(): void {
                a.b[0].c = 1;
            }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(41, 3, 2)),
                AccessModifier.Public,
                "test",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(41, 3, 2)), [
                    new ExpressionStatementNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(39, 2, 18)),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(38, 2, 17)),
                            BinaryExpressionKind.Assignment,
                            new MemberAccessExpressionNode(
                                new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(34, 2, 13)),
                                new ArrayAccessExpressionNode(
                                    new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(32, 2, 11)),
                                    new MemberAccessExpressionNode(
                                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(29, 2, 8)),
                                        new MemberAccessExpressionNode(new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(27, 2, 6)), "a"),
                                        "b"
                                    ),
                                    LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(30, 2, 9), new SourcePosition(31, 2, 10)), 0)
                                ),
                                "c"
                            ),
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(37, 2, 16), new SourcePosition(38, 2, 17)), 1)
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
            public main(): void {
                return a[0][1];
            }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(43, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(43, 3, 2)), [
                    new ReturnStatementNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(41, 2, 20)),
                        new ArrayAccessExpressionNode(
                            new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(40, 2, 19)),
                            new ArrayAccessExpressionNode(
                                new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(37, 2, 16)),
                                new MemberAccessExpressionNode(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(34, 2, 13)), "a"),
                                LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(35, 2, 14), new SourcePosition(36, 2, 15)), 0)
                            ),
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(38, 2, 17), new SourcePosition(39, 2, 18)), 1)
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
            public main(): void {
                return a.b.c;
            }
            """);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(41, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(41, 3, 2)), [
                    new ReturnStatementNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(39, 2, 18)),
                        new MemberAccessExpressionNode(
                            new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(38, 2, 17)),
                            new MemberAccessExpressionNode(
                                new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(36, 2, 15)),
                                new MemberAccessExpressionNode(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(34, 2, 13)), "a"),
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
            public main(): void {
                return a.b.;
            }
            """);

        var expected = new SyntaxTree([
            new FunctionDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(40, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(40, 3, 2)),
                    [
                        new ReturnStatementNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(38, 2, 17)),
                            new FakeExpressionNode(
                                new SourceSpan(new SourcePosition(37, 2, 16), new SourcePosition(38, 2, 17))
                            )
                        ),
                    ]
                )
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticIds.P0013_ExpectedIdentifier,
            DiagnosticSeverity.Error,
            file,
            new SourceSpan(new SourcePosition(37, 2, 16), new SourcePosition(38, 2, 17)),
            "Expected an identifier.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMemberAccessNestedCallTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                return a.b().c;
            }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(43, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(43, 3, 2)), [
                    new ReturnStatementNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(41, 2, 20)),
                        new MemberAccessExpressionNode(
                            new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(40, 2, 19)),
                            new CallExpressionNode(
                                new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(38, 2, 17)),
                                new MemberAccessExpressionNode(
                                    new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(36, 2, 15)),
                                    new MemberAccessExpressionNode(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(34, 2, 13)), "a"),
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
            public main(): void {
                f(1)(2);
            }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(36, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(36, 3, 2)), [
                    new ExpressionStatementNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(34, 2, 13)),
                        new CallExpressionNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(33, 2, 12)),
                            new CallExpressionNode(
                                new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(30, 2, 9)),
                                new MemberAccessExpressionNode(new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(27, 2, 6)), "f"),
                                [LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(28, 2, 7), new SourcePosition(29, 2, 8)), 1)]
                            ),
                            [LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(31, 2, 10), new SourcePosition(32, 2, 11)), 2)]
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
            public main(): void {
                return new Test().a;
            }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(48, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(48, 3, 2)), [
                    new ReturnStatementNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(46, 2, 25)),
                        new MemberAccessExpressionNode(
                            new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(45, 2, 24)),
                            new NewObjectExpressionNode(
                                new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(43, 2, 22)),
                                new TypeNode(new SourceSpan(new SourcePosition(37, 2, 16), new SourcePosition(41, 2, 20)), "Test"),
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
            public main(): void {
                return new i32[0].size;
            }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(51, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(51, 3, 2)), [
                    new ReturnStatementNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(49, 2, 28)),
                        new MemberAccessExpressionNode(
                            new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(48, 2, 27)),
                            new NewArrayExpressionNode(
                                new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(43, 2, 22)),
                                new ArrayTypeNode(new SourceSpan(new SourcePosition(37, 2, 16), new SourcePosition(40, 2, 19)), new TypeNode(new SourceSpan(new SourcePosition(37, 2, 16), new SourcePosition(40, 2, 19)), "i32")),
                                LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(41, 2, 20), new SourcePosition(42, 2, 21)), 0)
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
            public main(): void {
                return 1.toString();
            }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(48, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(48, 3, 2)), [
                    new ReturnStatementNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(46, 2, 25)),
                        new CallExpressionNode(
                            new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(45, 2, 24)),
                            new MemberAccessExpressionNode(
                                new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(43, 2, 22)),
                                LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(34, 2, 13)), 1),
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
            public main(): void {
                return 1 + 2.toString();
            }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(52, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(52, 3, 2)), [
                    new ReturnStatementNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(50, 2, 29)),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(49, 2, 28)),
                            BinaryExpressionKind.Addition,
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(34, 2, 13)), 1),
                            new CallExpressionNode(
                                new SourceSpan(new SourcePosition(37, 2, 16), new SourcePosition(49, 2, 28)),
                                new MemberAccessExpressionNode(
                                    new SourceSpan(new SourcePosition(37, 2, 16), new SourcePosition(47, 2, 26)),
                                    LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(37, 2, 16), new SourcePosition(38, 2, 17)), 2),
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
            public test(t: (i32, string)): i32 {
                return t.0;
            }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(54, 3, 2)),
                AccessModifier.Public,
                "test",
                [
                    new ParameterNode(
                        new SourceSpan(new SourcePosition(12, 1, 13), new SourcePosition(28, 1, 29)),
                        "t",
                        new TupleTypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(28, 1, 29)), [
                            new TypeNode(new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(19, 1, 20)), "i32"),
                            new TypeNode(new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(27, 1, 28)), "string")
                        ])
                    )
                ],
                new TypeNode(new SourceSpan(new SourcePosition(31, 1, 32), new SourcePosition(34, 1, 35)), "i32"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(35, 1, 36), new SourcePosition(54, 3, 2)), [
                    new ReturnStatementNode(
                        new SourceSpan(new SourcePosition(41, 2, 5), new SourcePosition(52, 2, 16)),
                        new MemberAccessExpressionNode(
                            new SourceSpan(new SourcePosition(48, 2, 12), new SourcePosition(51, 2, 15)),
                            new MemberAccessExpressionNode(new SourceSpan(new SourcePosition(48, 2, 12), new SourcePosition(49, 2, 13)), "t"),
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
            public test(t: (i32, string)): i32 {
                return t.0x;
            }
            """);

        var expected = new SyntaxTree([
            new FunctionDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(55, 3, 2)),
                AccessModifier.Public,
                "test",
                [
                    new ParameterNode(
                        new SourceSpan(new SourcePosition(12, 1, 13), new SourcePosition(28, 1, 29)),
                        "t",
                        new TupleTypeNode(
                            new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(28, 1, 29)),
                            [
                                new TypeNode(
                                    new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(19, 1, 20)),
                                    "i32"
                                ),
                                new TypeNode(
                                    new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(27, 1, 28)),
                                    "string"
                                ),
                            ]
                        )
                    )
                ],
                new TypeNode(
                    new SourceSpan(new SourcePosition(31, 1, 32), new SourcePosition(34, 1, 35)),
                    "i32"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(35, 1, 36), new SourcePosition(55, 3, 2)),
                    [
                        new ReturnStatementNode(
                            new SourceSpan(new SourcePosition(41, 2, 5), new SourcePosition(51, 2, 15)),
                            new MemberAccessExpressionNode(
                                new SourceSpan(new SourcePosition(48, 2, 12), new SourcePosition(51, 2, 15)),
                                new MemberAccessExpressionNode(
                                    new SourceSpan(new SourcePosition(48, 2, 12), new SourcePosition(49, 2, 13)),
                                    "t"
                                ),
                                "0"
                            )
                        ),
                        new ExpressionStatementNode(
                            new SourceSpan(new SourcePosition(51, 2, 15), new SourcePosition(53, 2, 17)),
                            new MemberAccessExpressionNode(
                                new SourceSpan(new SourcePosition(51, 2, 15), new SourcePosition(52, 2, 16)),
                                "x"
                            )
                        )
                    ]
                )
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticIds.P0001_MissingToken,
            DiagnosticSeverity.Error,
            file,
            new SourcePosition(51, 2, 15).ToSpan(),
            "Expected ';'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }
}