using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseExpressionTests
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
    public void ParseVariableTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var x: i32 = 5;
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(43, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(43, 3, 2)),
                    [
                        new VariableDeclarationNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(41, 2, 20)),
                            "x",
                            new TypeNode(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(36, 2, 15)), "i32"),
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(39, 2, 18), new SourcePosition(40, 2, 19)), 5)
                        )
                    ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseUnaryPlusTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var x: i32 = +2;
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(44, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(44, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(42, 2, 21)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(36, 2, 15)), "i32"),
                        new UnaryExpressionNode(
                            new SourceSpan(new SourcePosition(39, 2, 18), new SourcePosition(41, 2, 20)),
                            UnaryExpressionKind.UnaryPlus,
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(40, 2, 19), new SourcePosition(41, 2, 20)), 2)
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseUnaryMinusTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var x: i32 = -2;
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(44, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(44, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(42, 2, 21)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(36, 2, 15)), "i32"),
                        new UnaryExpressionNode(
                            new SourceSpan(new SourcePosition(39, 2, 18), new SourcePosition(41, 2, 20)),
                            UnaryExpressionKind.UnaryMinus,
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(40, 2, 19), new SourcePosition(41, 2, 20)), 2)
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseLogicalNotTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var x: i32 = !2;
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(44, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(44, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(42, 2, 21)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(36, 2, 15)), "i32"),
                        new UnaryExpressionNode(
                            new SourceSpan(new SourcePosition(39, 2, 18), new SourcePosition(41, 2, 20)),
                            UnaryExpressionKind.LogicalNot,
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(40, 2, 19), new SourcePosition(41, 2, 20)), 2)
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseBitwiseNotTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var x: i32 = ~2;
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(44, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(44, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(42, 2, 21)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(36, 2, 15)), "i32"),
                        new UnaryExpressionNode(
                            new SourceSpan(new SourcePosition(39, 2, 18), new SourcePosition(41, 2, 20)),
                            UnaryExpressionKind.BitwiseNot,
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(40, 2, 19), new SourcePosition(41, 2, 20)), 2)
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void MultipleUnaryOperatorsTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var x: i32 = ~+2;
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(45, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(45, 3, 2)),
                    [
                        new VariableDeclarationNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(43, 2, 22)),
                            "x",
                            new TypeNode(
                                new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(36, 2, 15)),
                                "i32"
                            ),
                            new UnaryExpressionNode(
                                new SourceSpan(new SourcePosition(39, 2, 18), new SourcePosition(42, 2, 21)),
                                UnaryExpressionKind.BitwiseNot,
                                new UnaryExpressionNode(
                                    new SourceSpan(new SourcePosition(40, 2, 19), new SourcePosition(42, 2, 21)),
                                    UnaryExpressionKind.UnaryPlus,
                                    LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(41, 2, 20), new SourcePosition(42, 2, 21)), 2)
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
    [TestCase("+", BinaryExpressionKind.Addition)]
    [TestCase("-", BinaryExpressionKind.Subtraction)]
    [TestCase("*", BinaryExpressionKind.Multiplication)]
    [TestCase("/", BinaryExpressionKind.Division)]
    [TestCase("%", BinaryExpressionKind.Modulus)]
    public void ParseBinaryNumberTest(string @operator, BinaryExpressionKind kind)
    {
        var (tree, diagnostics) = Parse(
            $$"""
              public main(): void {
                  var x: i32 = 2 {{@operator}} 2;
              }
              """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(47, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(47, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(45, 2, 24)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(36, 2, 15)), "i32"),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(39, 2, 18), new SourcePosition(44, 2, 23)),
                            kind,
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(39, 2, 18), new SourcePosition(40, 2, 19)), 2),
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(43, 2, 22), new SourcePosition(44, 2, 23)), 2)
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseBitwiseAndTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var x: i32 = 2 & 2;
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(47, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(47, 3, 2)),
                    [
                        new VariableDeclarationNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(45, 2, 24)),
                            "x",
                            new TypeNode(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(36, 2, 15)), "i32"),
                            new BinaryExpressionNode(
                                new SourceSpan(new SourcePosition(39, 2, 18), new SourcePosition(44, 2, 23)),
                                BinaryExpressionKind.BitwiseAnd,
                                LiteralExpressionNode.Integer(
                                    new SourceSpan(new SourcePosition(39, 2, 18), new SourcePosition(40, 2, 19)),
                                    2
                                ),
                                LiteralExpressionNode.Integer(
                                    new SourceSpan(new SourcePosition(43, 2, 22), new SourcePosition(44, 2, 23)),
                                    2
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
    public void ParseBitwiseOrTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var x: i32 = 2 | 2;
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(47, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(47, 3, 2)),
                    [
                        new VariableDeclarationNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(45, 2, 24)),
                            "x",
                            new TypeNode(
                                new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(36, 2, 15)),
                                "i32"
                            ),
                            new BinaryExpressionNode(
                                new SourceSpan(new SourcePosition(39, 2, 18), new SourcePosition(44, 2, 23)),
                                BinaryExpressionKind.BitwiseOr,
                                LiteralExpressionNode.Integer(
                                    new SourceSpan(new SourcePosition(39, 2, 18), new SourcePosition(40, 2, 19)),
                                    2
                                ),
                                LiteralExpressionNode.Integer(
                                    new SourceSpan(new SourcePosition(43, 2, 22), new SourcePosition(44, 2, 23)),
                                    2
                                )
                            )
                        )
                    ]
                )
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseBitwiseXorTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var x: i32 = 2 ^ 2;
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(47, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(47, 3, 2)),
                    [
                        new VariableDeclarationNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(45, 2, 24)),
                            "x",
                            new TypeNode(
                                new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(36, 2, 15)),
                                "i32"
                            ),
                            new BinaryExpressionNode(
                                new SourceSpan(new SourcePosition(39, 2, 18), new SourcePosition(44, 2, 23)),
                                BinaryExpressionKind.BitwiseXor,
                                LiteralExpressionNode.Integer(
                                    new SourceSpan(new SourcePosition(39, 2, 18), new SourcePosition(40, 2, 19)),
                                    2
                                ),
                                LiteralExpressionNode.Integer(
                                    new SourceSpan(new SourcePosition(43, 2, 22), new SourcePosition(44, 2, 23)),
                                    2
                                )
                            )
                        )
                    ]
                )
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseLogicalAndTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var x: bool = true && true;
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(55, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(55, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(53, 2, 32)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(37, 2, 16)), "bool"),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(40, 2, 19), new SourcePosition(52, 2, 31)),
                            BinaryExpressionKind.ConditionalAnd,
                            LiteralExpressionNode.True(new SourceSpan(new SourcePosition(40, 2, 19), new SourcePosition(44, 2, 23))),
                            LiteralExpressionNode.True(new SourceSpan(new SourcePosition(48, 2, 27), new SourcePosition(52, 2, 31)))
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseLogicalOrTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var x: bool = true || true;
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(55, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(55, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(53, 2, 32)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(37, 2, 16)), "bool"),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(40, 2, 19), new SourcePosition(52, 2, 31)),
                            BinaryExpressionKind.ConditionalOr,
                            LiteralExpressionNode.True(new SourceSpan(new SourcePosition(40, 2, 19), new SourcePosition(44, 2, 23))),
                            LiteralExpressionNode.True(new SourceSpan(new SourcePosition(48, 2, 27), new SourcePosition(52, 2, 31)))
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseEqualityTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var x: bool = true == true;
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(55, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(55, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(53, 2, 32)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(37, 2, 16)), "bool"),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(40, 2, 19), new SourcePosition(52, 2, 31)),
                            BinaryExpressionKind.Equality,
                            LiteralExpressionNode.True(new SourceSpan(new SourcePosition(40, 2, 19), new SourcePosition(44, 2, 23))),
                            LiteralExpressionNode.True(new SourceSpan(new SourcePosition(48, 2, 27), new SourcePosition(52, 2, 31)))
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseInequalityTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var x: bool = true != true;
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(55, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(55, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(53, 2, 32)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(37, 2, 16)), "bool"),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(40, 2, 19), new SourcePosition(52, 2, 31)),
                            BinaryExpressionKind.Inequality,
                            LiteralExpressionNode.True(new SourceSpan(new SourcePosition(40, 2, 19), new SourcePosition(44, 2, 23))),
                            LiteralExpressionNode.True(new SourceSpan(new SourcePosition(48, 2, 27), new SourcePosition(52, 2, 31)))
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseLessThanTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var x: bool = 2 < 2;
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(48, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(48, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(46, 2, 25)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(37, 2, 16)), "bool"),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(40, 2, 19), new SourcePosition(45, 2, 24)),
                            BinaryExpressionKind.LessThan,
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(40, 2, 19), new SourcePosition(41, 2, 20)), 2),
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(44, 2, 23), new SourcePosition(45, 2, 24)), 2)
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseLessThanOrEqualTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var x: bool = 2 <= 2;
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(49, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(49, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(47, 2, 26)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(37, 2, 16)), "bool"),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(40, 2, 19), new SourcePosition(46, 2, 25)),
                            BinaryExpressionKind.LessThanOrEqual,
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(40, 2, 19), new SourcePosition(41, 2, 20)), 2),
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(45, 2, 24), new SourcePosition(46, 2, 25)), 2)
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseGreaterThanTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var x: bool = 2 > 2;
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(48, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(48, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(46, 2, 25)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(37, 2, 16)), "bool"),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(40, 2, 19), new SourcePosition(45, 2, 24)),
                            BinaryExpressionKind.GreaterThan,
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(40, 2, 19), new SourcePosition(41, 2, 20)), 2),
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(44, 2, 23), new SourcePosition(45, 2, 24)), 2)
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseGreaterThanOrEqualTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var x: bool = 2 >= 2;
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(49, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(49, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(47, 2, 26)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(37, 2, 16)), "bool"),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(40, 2, 19), new SourcePosition(46, 2, 25)),
                            BinaryExpressionKind.GreaterThanOrEqual,
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(40, 2, 19), new SourcePosition(41, 2, 20)), 2),
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(45, 2, 24), new SourcePosition(46, 2, 25)), 2)
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseAssignmentTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                x = 1;
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(34, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(34, 3, 2)), [
                    new ExpressionStatementNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(32, 2, 11)),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(31, 2, 10)),
                            BinaryExpressionKind.Assignment,
                            new MemberAccessExpressionNode(new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(27, 2, 6)), "x"),
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(30, 2, 9), new SourcePosition(31, 2, 10)), 1)
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    [TestCase("+=", BinaryExpressionKind.AdditionAssignment)]
    [TestCase("-=", BinaryExpressionKind.SubtractionAssignment)]
    [TestCase("*=", BinaryExpressionKind.MultiplicationAssignment)]
    [TestCase("/=", BinaryExpressionKind.DivisionAssignment)]
    [TestCase("%=", BinaryExpressionKind.ModulusAssignment)]
    [TestCase("&=", BinaryExpressionKind.BitwiseAndAssignment)]
    [TestCase("|=", BinaryExpressionKind.BitwiseOrAssignment)]
    [TestCase("^=", BinaryExpressionKind.BitwiseXorAssignment)]
    public void ParseAssignment2Test(string @operator, BinaryExpressionKind kind)
    {
        var (tree, diagnostics) = Parse(
            $$"""
              public main(): void {
                  x {{@operator}} 1;
              }
              """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(35, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(35, 3, 2)), [
                    new ExpressionStatementNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(33, 2, 12)),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(32, 2, 11)),
                            kind,
                            new MemberAccessExpressionNode(new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(27, 2, 6)), "x"),
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(31, 2, 10), new SourcePosition(32, 2, 11)), 1)
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseParenTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var x: i32 = (1 + 2) * 3;
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(53, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(53, 3, 2)),
                    [
                        new VariableDeclarationNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(51, 2, 30)),
                            "x",
                            new TypeNode(
                                new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(36, 2, 15)),
                                "i32"
                            ),
                            new BinaryExpressionNode(
                                new SourceSpan(new SourcePosition(40, 2, 19), new SourcePosition(50, 2, 29)),
                                BinaryExpressionKind.Multiplication,
                                new BinaryExpressionNode(
                                    new SourceSpan(new SourcePosition(40, 2, 19), new SourcePosition(45, 2, 24)),
                                    BinaryExpressionKind.Addition,
                                    LiteralExpressionNode.Integer(
                                        new SourceSpan(new SourcePosition(40, 2, 19), new SourcePosition(41, 2, 20)),
                                        1
                                    ),
                                    LiteralExpressionNode.Integer(
                                        new SourceSpan(new SourcePosition(44, 2, 23), new SourcePosition(45, 2, 24)),
                                        2
                                    )
                                ),
                                LiteralExpressionNode.Integer(
                                    new SourceSpan(new SourcePosition(49, 2, 28), new SourcePosition(50, 2, 29)),
                                    3
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
    public void ParseVariableExpressionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(y: i32): void {
                var x: i32 = 2 * y;
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(53, 3, 2)),
                AccessModifier.Public,
                "main",
                [new ParameterNode(new SourceSpan(new SourcePosition(12, 1, 13), new SourcePosition(18, 1, 19)), "y", new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(18, 1, 19)), "i32"))],
                new TypeNode(new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(25, 1, 26)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(26, 1, 27), new SourcePosition(53, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(32, 2, 5), new SourcePosition(51, 2, 24)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(39, 2, 12), new SourcePosition(42, 2, 15)), "i32"),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(45, 2, 18), new SourcePosition(50, 2, 23)),
                            BinaryExpressionKind.Multiplication,
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(45, 2, 18), new SourcePosition(46, 2, 19)), 2),
                            new MemberAccessExpressionNode(new SourceSpan(new SourcePosition(49, 2, 22), new SourcePosition(50, 2, 23)), "y")
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParsePrecedenceTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var x: i32 = true || true && false | false ^ false & true == 1 + 2 * 3 < 10;
            }
            """);

        var variableDeclarationNode = new VariableDeclarationNode(
            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(102, 2, 81)),
            "x",
            new TypeNode(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(36, 2, 15)), "i32"),
            new BinaryExpressionNode(
                new SourceSpan(new SourcePosition(39, 2, 18), new SourcePosition(101, 2, 80)),
                BinaryExpressionKind.ConditionalOr,
                LiteralExpressionNode.True(new SourceSpan(new SourcePosition(39, 2, 18), new SourcePosition(43, 2, 22))),
                new BinaryExpressionNode(
                    new SourceSpan(new SourcePosition(47, 2, 26), new SourcePosition(101, 2, 80)),
                    BinaryExpressionKind.ConditionalAnd,
                    LiteralExpressionNode.True(new SourceSpan(new SourcePosition(47, 2, 26), new SourcePosition(51, 2, 30))),
                    new BinaryExpressionNode(
                        new SourceSpan(new SourcePosition(55, 2, 34), new SourcePosition(101, 2, 80)),
                        BinaryExpressionKind.BitwiseOr,
                        LiteralExpressionNode.False(new SourceSpan(new SourcePosition(55, 2, 34), new SourcePosition(60, 2, 39))),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(63, 2, 42), new SourcePosition(101, 2, 80)),
                            BinaryExpressionKind.BitwiseXor,
                            LiteralExpressionNode.False(new SourceSpan(new SourcePosition(63, 2, 42), new SourcePosition(68, 2, 47))),
                            new BinaryExpressionNode(
                                new SourceSpan(new SourcePosition(71, 2, 50), new SourcePosition(101, 2, 80)),
                                BinaryExpressionKind.BitwiseAnd,
                                LiteralExpressionNode.False(new SourceSpan(new SourcePosition(71, 2, 50), new SourcePosition(76, 2, 55))),
                                new BinaryExpressionNode(
                                    new SourceSpan(new SourcePosition(79, 2, 58), new SourcePosition(101, 2, 80)),
                                    BinaryExpressionKind.Equality,
                                    LiteralExpressionNode.True(new SourceSpan(new SourcePosition(79, 2, 58), new SourcePosition(83, 2, 62))),
                                    new BinaryExpressionNode(
                                        new SourceSpan(new SourcePosition(87, 2, 66), new SourcePosition(101, 2, 80)),
                                        BinaryExpressionKind.LessThan,
                                        new BinaryExpressionNode(
                                            new SourceSpan(new SourcePosition(87, 2, 66), new SourcePosition(96, 2, 75)),
                                            BinaryExpressionKind.Addition,
                                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(87, 2, 66), new SourcePosition(88, 2, 67)), 1),
                                            new BinaryExpressionNode(
                                                new SourceSpan(new SourcePosition(91, 2, 70), new SourcePosition(96, 2, 75)),
                                                BinaryExpressionKind.Multiplication,
                                                LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(91, 2, 70), new SourcePosition(92, 2, 71)), 2),
                                                LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(95, 2, 74), new SourcePosition(96, 2, 75)), 3)
                                            )
                                        ),
                                        LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(99, 2, 78), new SourcePosition(101, 2, 80)), 10)
                                    )
                                )
                            )
                        )
                    )
                )
            )
        );
        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(104, 3, 2)),
                AccessModifier.Public,
                "main", [], new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"), new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(104, 3, 2)), [variableDeclarationNode])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void TupleExpressionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                return (1, 2);
            }
            """);
        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(42, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(42, 3, 2)), [
                    new ReturnStatementNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(40, 2, 19)),
                        new TupleExpressionNode(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(39, 2, 18)), [
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(34, 2, 13), new SourcePosition(35, 2, 14)), 1),
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(37, 2, 16), new SourcePosition(38, 2, 17)), 2),
                        ])
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void TupleExpressionMissingCloseParenTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                return (1, 2;
            }
            """);

        var expected = new SyntaxTree(file, [
            new FunctionDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(41, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(41, 3, 2)),
                    [
                        new ReturnStatementNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(39, 2, 18)),
                            new TupleExpressionNode(
                                new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(38, 2, 17)),
                                [
                                    LiteralExpressionNode.Integer(
                                        new SourceSpan(new SourcePosition(34, 2, 13), new SourcePosition(35, 2, 14)),
                                        1
                                    ),
                                    LiteralExpressionNode.Integer(
                                        new SourceSpan(new SourcePosition(37, 2, 16), new SourcePosition(38, 2, 17)),
                                        2
                                    ),
                                ]
                            )
                        )
                    ]
                )
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticIds.P0001_MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(38, 2, 17).ToSpan()),
            "Expected ')'."
        );

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseNewArrayTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                return new i32[10];
            }
            """);
        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(47, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(47, 3, 2)), [
                    new ReturnStatementNode(
                        new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(45, 2, 24)),
                        new NewArrayExpressionNode(
                            new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(44, 2, 23)),
                            new ArrayTypeNode(
                                new SourceSpan(new SourcePosition(37, 2, 16), new SourcePosition(40, 2, 19)),
                                new TypeNode(new SourceSpan(new SourcePosition(37, 2, 16), new SourcePosition(40, 2, 19)), "i32")
                            ),
                            LiteralExpressionNode.Integer(
                                new SourceSpan(new SourcePosition(41, 2, 20), new SourcePosition(43, 2, 22)),
                                10
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
    public void ParseAsExpressionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(a: i32): i8 {
                return a is i8;
            }
            """);
        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(47, 3, 2)),
                AccessModifier.Public,
                "test",
                [
                    new ParameterNode(
                        new SourceSpan(new SourcePosition(12, 1, 13), new SourcePosition(18, 1, 19)),
                        "a",
                        new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(18, 1, 19)), "i32")
                    )
                ],
                new TypeNode(new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(23, 1, 24)), "i8"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(24, 1, 25), new SourcePosition(47, 3, 2)), [
                    new ReturnStatementNode(
                        new SourceSpan(new SourcePosition(30, 2, 5), new SourcePosition(45, 2, 20)),
                        new IsExpressionNode(
                            new MemberAccessExpressionNode(new SourceSpan(new SourcePosition(37, 2, 12), new SourcePosition(38, 2, 13)), "a"),
                            new TypeNode(new SourceSpan(new SourcePosition(42, 2, 17), new SourcePosition(44, 2, 19)), "i8")
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseAsExpressionMissingTypeTest()
    {
        const string code =
            """
            public test(a: i32): i8 {
                return a is;
            }
            """;
        var (tree, diagnostics) = Parse(code);

        var expected = new SyntaxTree(file, [
            new FunctionDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(44, 3, 2)),
                AccessModifier.Public,
                "test",
                [
                    new ParameterNode(
                        new SourceSpan(new SourcePosition(12, 1, 13), new SourcePosition(18, 1, 19)),
                        "a",
                        new TypeNode(
                            new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(18, 1, 19)),
                            "i32"
                        )
                    )
                ],
                new TypeNode(
                    new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(23, 1, 24)),
                    "i8"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(24, 1, 25), new SourcePosition(44, 3, 2)),
                    [
                        new ReturnStatementNode(
                            new SourceSpan(new SourcePosition(30, 2, 5), new SourcePosition(42, 2, 17)),
                            new IsExpressionNode(
                                new MemberAccessExpressionNode(
                                    new SourceSpan(new SourcePosition(37, 2, 12), new SourcePosition(38, 2, 13)),
                                    "a"
                                ),
                                new FakeTypeNode(
                                    new SourcePosition(41, 2, 16).ToSpan(),
                                    "<>_0"
                                )
                            )
                        )
                    ]
                )
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticIds.P0003_ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(41, 2, 16).ToSpan()),
            "Expected a type.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseCastExpressionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(a: i32): i8 {
                return (i8)a;
            }
            """);
        var expected = new SyntaxTree(file, [
            new FunctionDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(45, 3, 2)),
                AccessModifier.Public,
                "test",
                [
                    new ParameterNode(
                        new SourceSpan(new SourcePosition(12, 1, 13), new SourcePosition(18, 1, 19)),
                        "a",
                        new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(18, 1, 19)), "i32")
                    )
                ],
                new TypeNode(new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(23, 1, 24)), "i8"),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(24, 1, 25), new SourcePosition(45, 3, 2)),
                    [
                        new ReturnStatementNode(
                            new SourceSpan(new SourcePosition(30, 2, 5), new SourcePosition(43, 2, 18)),
                            new CastExpressionNode(
                                new SourceSpan(new SourcePosition(37, 2, 12), new SourcePosition(42, 2, 17)),
                                new TypeNode(new SourceSpan(new SourcePosition(38, 2, 13), new SourcePosition(40, 2, 15)), "i8"),
                                new MemberAccessExpressionNode(new SourceSpan(new SourcePosition(41, 2, 16), new SourcePosition(42, 2, 17)), "a")
                            )
                        )
                    ]
                )
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseCastExpressionMissingCloseParenTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(a: i32): i8 {
                return (i8 a;
            }
            """);

        var expected = new SyntaxTree(file, [
            new FunctionDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(45, 3, 2)),
                AccessModifier.Public,
                "test",
                [
                    new ParameterNode(
                        new SourceSpan(new SourcePosition(12, 1, 13), new SourcePosition(18, 1, 19)),
                        "a",
                        new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(18, 1, 19)), "i32")
                    )
                ],
                new TypeNode(new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(23, 1, 24)), "i8"),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(24, 1, 25), new SourcePosition(45, 3, 2)),
                    [
                        new ReturnStatementNode(
                            new SourceSpan(new SourcePosition(30, 2, 5), new SourcePosition(43, 2, 18)),
                            new CastExpressionNode(
                                new SourceSpan(new SourcePosition(37, 2, 12), new SourcePosition(42, 2, 17)),
                                new TypeNode(new SourceSpan(new SourcePosition(38, 2, 13), new SourcePosition(40, 2, 15)), "i8"),
                                new MemberAccessExpressionNode(new SourceSpan(new SourcePosition(41, 2, 16), new SourcePosition(42, 2, 17)), "a")
                            )
                        )
                    ]
                )
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticIds.P0001_MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(41, 2, 16).ToSpan()),
            "Expected ')'."
        );

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseCastExpressionMissingExpressionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(a: i32): i8 {
                return (i8);
            }
            """);

        var expected = new SyntaxTree(file, [
            new FunctionDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(44, 3, 2)),
                AccessModifier.Public,
                "test",
                [
                    new ParameterNode(
                        new SourceSpan(new SourcePosition(12, 1, 13), new SourcePosition(18, 1, 19)),
                        "a",
                        new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(18, 1, 19)), "i32")
                    )
                ],
                new TypeNode(new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(23, 1, 24)), "i8"),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(24, 1, 25), new SourcePosition(44, 3, 2)),
                    [
                        new ReturnStatementNode(
                            new SourceSpan(new SourcePosition(30, 2, 5), new SourcePosition(42, 2, 17)),
                            new CastExpressionNode(
                                new SourceSpan(new SourcePosition(37, 2, 12), new SourcePosition(41, 2, 16)),
                                new TypeNode(
                                    new SourceSpan(new SourcePosition(38, 2, 13), new SourcePosition(40, 2, 15)),
                                    "i8"
                                ),
                                new FakeExpressionNode(
                                    new SourcePosition(41, 2, 16).ToSpan()
                                )
                            )
                        )
                    ]
                )
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticIds.P0009_ExpectedExpression,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(41, 2, 16).ToSpan()),
            "Expected an expression."
        );

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseFloatingNumberTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(): f64 {
                return 3.14;
            }
            """);
        var expected = new SyntaxTree(file, [
            new FunctionDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(39, 3, 2)),
                AccessModifier.Public,
                "test",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(18, 1, 19)), "f64"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(19, 1, 20), new SourcePosition(39, 3, 2)), [
                    new ReturnStatementNode(
                        new SourceSpan(new SourcePosition(25, 2, 5), new SourcePosition(37, 2, 17)),
                        LiteralExpressionNode.Float(new SourceSpan(new SourcePosition(32, 2, 12), new SourcePosition(36, 2, 16)), 3.14)
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }
}