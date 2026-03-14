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
            namespace Test1;

            public main(): void {
                var x: i32 = 5;
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
                    new TypeRefNode(
                        default,
                        "void"
                    ),
                    new BlockStatementNode(
                        default,
                        [
                            new VariableDeclarationNode(
                                default,
                                "x",
                                new TypeRefNode(default, "i32"),
                                LiteralExpressionNode.Integer(default, 5)
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
            namespace Test1;

            public main(): void {
                var x: i32 = +2;
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
                        new VariableDeclarationNode(
                            default,
                            "x",
                            new TypeRefNode(default, "i32"),
                            new UnaryExpressionNode(
                                default,
                                UnaryExpressionKind.UnaryPlus,
                                LiteralExpressionNode.Integer(default, 2)
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
            namespace Test1;

            public main(): void {
                var x: i32 = -2;
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
                        new VariableDeclarationNode(
                            default,
                            "x",
                            new TypeRefNode(default, "i32"),
                            new UnaryExpressionNode(
                                default,
                                UnaryExpressionKind.UnaryMinus,
                                LiteralExpressionNode.Integer(default, 2)
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
            namespace Test1;

            public main(): void {
                var x: i32 = !2;
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
                        new VariableDeclarationNode(
                            default,
                            "x",
                            new TypeRefNode(default, "i32"),
                            new UnaryExpressionNode(
                                default,
                                UnaryExpressionKind.LogicalNot,
                                LiteralExpressionNode.Integer(default, 2)
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
            namespace Test1;

            public main(): void {
                var x: i32 = ~2;
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
                        new VariableDeclarationNode(
                            default,
                            "x",
                            new TypeRefNode(default, "i32"),
                            new UnaryExpressionNode(
                                default,
                                UnaryExpressionKind.BitwiseNot,
                                LiteralExpressionNode.Integer(default, 2)
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
            namespace Test1;

            public main(): void {
                var x: i32 = ~+2;
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
                    new TypeRefNode(
                        default,
                        "void"
                    ),
                    new BlockStatementNode(
                        default,
                        [
                            new VariableDeclarationNode(
                                default,
                                "x",
                                new TypeRefNode(
                                    default,
                                    "i32"
                                ),
                                new UnaryExpressionNode(
                                    default,
                                    UnaryExpressionKind.BitwiseNot,
                                    new UnaryExpressionNode(
                                        default,
                                        UnaryExpressionKind.UnaryPlus,
                                        LiteralExpressionNode.Integer(default, 2)
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
              namespace Test1;

              public main(): void {
                  var x: i32 = 2 {{@operator}} 2;
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
                        new VariableDeclarationNode(
                            default,
                            "x",
                            new TypeRefNode(default, "i32"),
                            new BinaryExpressionNode(
                                default,
                                kind,
                                LiteralExpressionNode.Integer(default, 2),
                                LiteralExpressionNode.Integer(default, 2)
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
            namespace Test1;

            public main(): void {
                var x: i32 = 2 & 2;
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
                    new BlockStatementNode(
                        default,
                        [
                            new VariableDeclarationNode(
                                default,
                                "x",
                                new TypeRefNode(default, "i32"),
                                new BinaryExpressionNode(
                                    default,
                                    BinaryExpressionKind.BitwiseAnd,
                                    LiteralExpressionNode.Integer(
                                        default,
                                        2
                                    ),
                                    LiteralExpressionNode.Integer(
                                        default,
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
            namespace Test1;

            public main(): void {
                var x: i32 = 2 | 2;
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
                    new TypeRefNode(
                        default,
                        "void"
                    ),
                    new BlockStatementNode(
                        default,
                        [
                            new VariableDeclarationNode(
                                default,
                                "x",
                                new TypeRefNode(
                                    default,
                                    "i32"
                                ),
                                new BinaryExpressionNode(
                                    default,
                                    BinaryExpressionKind.BitwiseOr,
                                    LiteralExpressionNode.Integer(
                                        default,
                                        2
                                    ),
                                    LiteralExpressionNode.Integer(
                                        default,
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
            namespace Test1;

            public main(): void {
                var x: i32 = 2 ^ 2;
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
                    new TypeRefNode(
                        default,
                        "void"
                    ),
                    new BlockStatementNode(
                        default,
                        [
                            new VariableDeclarationNode(
                                default,
                                "x",
                                new TypeRefNode(
                                    default,
                                    "i32"
                                ),
                                new BinaryExpressionNode(
                                    default,
                                    BinaryExpressionKind.BitwiseXor,
                                    LiteralExpressionNode.Integer(
                                        default,
                                        2
                                    ),
                                    LiteralExpressionNode.Integer(
                                        default,
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
            namespace Test1;

            public main(): void {
                var x: bool = true && true;
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
                        new VariableDeclarationNode(
                            default,
                            "x",
                            new TypeRefNode(default, "bool"),
                            new BinaryExpressionNode(
                                default,
                                BinaryExpressionKind.ConditionalAnd,
                                LiteralExpressionNode.True(default),
                                LiteralExpressionNode.True(default)
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
            namespace Test1;

            public main(): void {
                var x: bool = true || true;
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
                        new VariableDeclarationNode(
                            default,
                            "x",
                            new TypeRefNode(default, "bool"),
                            new BinaryExpressionNode(
                                default,
                                BinaryExpressionKind.ConditionalOr,
                                LiteralExpressionNode.True(default),
                                LiteralExpressionNode.True(default)
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
            namespace Test1;

            public main(): void {
                var x: bool = true == true;
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
                        new VariableDeclarationNode(
                            default,
                            "x",
                            new TypeRefNode(default, "bool"),
                            new BinaryExpressionNode(
                                default,
                                BinaryExpressionKind.Equality,
                                LiteralExpressionNode.True(default),
                                LiteralExpressionNode.True(default)
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
            namespace Test1;

            public main(): void {
                var x: bool = true != true;
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
                        new VariableDeclarationNode(
                            default,
                            "x",
                            new TypeRefNode(default, "bool"),
                            new BinaryExpressionNode(
                                default,
                                BinaryExpressionKind.Inequality,
                                LiteralExpressionNode.True(default),
                                LiteralExpressionNode.True(default)
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
            namespace Test1;

            public main(): void {
                var x: bool = 2 < 2;
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
                        new VariableDeclarationNode(
                            default,
                            "x",
                            new TypeRefNode(default, "bool"),
                            new BinaryExpressionNode(
                                default,
                                BinaryExpressionKind.LessThan,
                                LiteralExpressionNode.Integer(default, 2),
                                LiteralExpressionNode.Integer(default, 2)
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
            namespace Test1;

            public main(): void {
                var x: bool = 2 <= 2;
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
                        new VariableDeclarationNode(
                            default,
                            "x",
                            new TypeRefNode(default, "bool"),
                            new BinaryExpressionNode(
                                default,
                                BinaryExpressionKind.LessThanOrEqual,
                                LiteralExpressionNode.Integer(default, 2),
                                LiteralExpressionNode.Integer(default, 2)
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
            namespace Test1;

            public main(): void {
                var x: bool = 2 > 2;
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
                        new VariableDeclarationNode(
                            default,
                            "x",
                            new TypeRefNode(default, "bool"),
                            new BinaryExpressionNode(
                                default,
                                BinaryExpressionKind.GreaterThan,
                                LiteralExpressionNode.Integer(default, 2),
                                LiteralExpressionNode.Integer(default, 2)
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
            namespace Test1;

            public main(): void {
                var x: bool = 2 >= 2;
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
                        new VariableDeclarationNode(
                            default,
                            "x",
                            new TypeRefNode(default, "bool"),
                            new BinaryExpressionNode(
                                default,
                                BinaryExpressionKind.GreaterThanOrEqual,
                                LiteralExpressionNode.Integer(default, 2),
                                LiteralExpressionNode.Integer(default, 2)
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
            namespace Test1;

            public main(): void {
                x = 1;
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
                            new BinaryExpressionNode(
                                default,
                                BinaryExpressionKind.Assignment,
                                new MemberAccessExpressionNode(default, "x"),
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
              namespace Test1;

              public main(): void {
                  x {{@operator}} 1;
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
                            new BinaryExpressionNode(
                                default,
                                kind,
                                new MemberAccessExpressionNode(default, "x"),
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
    public void ParseParenTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                var x: i32 = (1 + 2) * 3;
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
                    new TypeRefNode(
                        default,
                        "void"
                    ),
                    new BlockStatementNode(
                        default,
                        [
                            new VariableDeclarationNode(
                                default,
                                "x",
                                new TypeRefNode(
                                    default,
                                    "i32"
                                ),
                                new BinaryExpressionNode(
                                    default,
                                    BinaryExpressionKind.Multiplication,
                                    new BinaryExpressionNode(
                                        default,
                                        BinaryExpressionKind.Addition,
                                        LiteralExpressionNode.Integer(
                                            default,
                                            1
                                        ),
                                        LiteralExpressionNode.Integer(
                                            default,
                                            2
                                        )
                                    ),
                                    LiteralExpressionNode.Integer(
                                        default,
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
            namespace Test1;

            public main(y: i32): void {
                var x: i32 = 2 * y;
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
                    [new ParameterNode(default, "y", new TypeRefNode(default, "i32"))],
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(default, [
                        new VariableDeclarationNode(
                            default,
                            "x",
                            new TypeRefNode(default, "i32"),
                            new BinaryExpressionNode(
                                default,
                                BinaryExpressionKind.Multiplication,
                                LiteralExpressionNode.Integer(default, 2),
                                new MemberAccessExpressionNode(default, "y")
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
            namespace Test1;

            public main(): void {
                var x: i32 = true || true && false | false ^ false & true == 1 + 2 * 3 < 10;
            }
            """);

        var variableDeclarationNode = new VariableDeclarationNode(
            default,
            "x",
            new TypeRefNode(default, "i32"),
            new BinaryExpressionNode(
                default,
                BinaryExpressionKind.ConditionalOr,
                LiteralExpressionNode.True(default),
                new BinaryExpressionNode(
                    default,
                    BinaryExpressionKind.ConditionalAnd,
                    LiteralExpressionNode.True(default),
                    new BinaryExpressionNode(
                        default,
                        BinaryExpressionKind.BitwiseOr,
                        LiteralExpressionNode.False(default),
                        new BinaryExpressionNode(
                            default,
                            BinaryExpressionKind.BitwiseXor,
                            LiteralExpressionNode.False(default),
                            new BinaryExpressionNode(
                                default,
                                BinaryExpressionKind.BitwiseAnd,
                                LiteralExpressionNode.False(default),
                                new BinaryExpressionNode(
                                    default,
                                    BinaryExpressionKind.Equality,
                                    LiteralExpressionNode.True(default),
                                    new BinaryExpressionNode(
                                        default,
                                        BinaryExpressionKind.LessThan,
                                        new BinaryExpressionNode(
                                            default,
                                            BinaryExpressionKind.Addition,
                                            LiteralExpressionNode.Integer(default, 1),
                                            new BinaryExpressionNode(
                                                default,
                                                BinaryExpressionKind.Multiplication,
                                                LiteralExpressionNode.Integer(default, 2),
                                                LiteralExpressionNode.Integer(default, 3)
                                            )
                                        ),
                                        LiteralExpressionNode.Integer(default, 10)
                                    )
                                )
                            )
                        )
                    )
                )
            )
        );
        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                FunctionDeclarationNode.Create(
                    default,
                    AccessModifier.Public,
                    "main", [], new TypeRefNode(default, "void"), new BlockStatementNode(default, [variableDeclarationNode])
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
            namespace Test1;

            public main(): void {
                return (1, 2);
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
                            new TupleExpressionNode(default, [
                                LiteralExpressionNode.Integer(default, 1),
                                LiteralExpressionNode.Integer(default, 2),
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
            namespace Test1;

            public main(): void {
                return (1, 2;
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
                                new TupleExpressionNode(
                                    default,
                                    [
                                        LiteralExpressionNode.Integer(
                                            default,
                                            1
                                        ),
                                        LiteralExpressionNode.Integer(
                                            default,
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
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(56, 4, 17).ToSpan()),
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
            namespace Test1;

            public main(): void {
                return new i32[10];
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
                            new NewArrayExpressionNode(
                                default,
                                new ArrayTypeNode(
                                    default,
                                    new TypeRefNode(default, "i32")
                                ),
                                LiteralExpressionNode.Integer(
                                    default,
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
            namespace Test1;

            public test(a: i32): i8 {
                return a is i8;
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
                            "a",
                            new TypeRefNode(default, "i32")
                        )
                    ],
                    new TypeRefNode(default, "i8"),
                    new BlockStatementNode(default, [
                        new ReturnStatementNode(
                            default,
                            new IsExpressionNode(
                                new MemberAccessExpressionNode(default, "a"),
                                new TypeRefNode(default, "i8")
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
            namespace Test1;

            public test(a: i32): i8 {
                return a is;
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
                            "a",
                            new TypeRefNode(
                                default,
                                "i32"
                            )
                        )
                    ],
                    new TypeRefNode(
                        default,
                        "i8"
                    ),
                    new BlockStatementNode(
                        default,
                        [
                            new ReturnStatementNode(
                                default,
                                new IsExpressionNode(
                                    new MemberAccessExpressionNode(
                                        default,
                                        "a"
                                    ),
                                    new FakeTypeNode(
                                        default,
                                        "<>_0"
                                    )
                                )
                            )
                        ]
                    )
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(59, 4, 16).ToSpan()),
            "Expected a type.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseCastExpressionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(a: i32): i8 {
                return (i8)a;
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
                            "a",
                            new TypeRefNode(default, "i32")
                        )
                    ],
                    new TypeRefNode(default, "i8"),
                    new BlockStatementNode(
                        default,
                        [
                            new ReturnStatementNode(
                                default,
                                new CastExpressionNode(
                                    default,
                                    new TypeRefNode(default, "i8"),
                                    new MemberAccessExpressionNode(default, "a")
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
            namespace Test1;

            public test(a: i32): i8 {
                return (i8 a;
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
                            "a",
                            new TypeRefNode(default, "i32")
                        )
                    ],
                    new TypeRefNode(default, "i8"),
                    new BlockStatementNode(
                        default,
                        [
                            new ReturnStatementNode(
                                default,
                                new CastExpressionNode(
                                    default,
                                    new TypeRefNode(default, "i8"),
                                    new MemberAccessExpressionNode(default, "a")
                                )
                            )
                        ]
                    )
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(59, 4, 16).ToSpan()),
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
            namespace Test1;

            public test(a: i32): i8 {
                return (i8);
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
                            "a",
                            new TypeRefNode(default, "i32")
                        )
                    ],
                    new TypeRefNode(default, "i8"),
                    new BlockStatementNode(
                        default,
                        [
                            new ReturnStatementNode(
                                default,
                                new CastExpressionNode(
                                    default,
                                    new TypeRefNode(
                                        default,
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
            DiagnosticId.P0009ExpectedExpression,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(59, 4, 16).ToSpan()),
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
            namespace Test1;

            public test(): f64 {
                return 3.14;
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
                    [],
                    new TypeRefNode(default, "f64"),
                    new BlockStatementNode(default, [
                        new ReturnStatementNode(
                            default,
                            LiteralExpressionNode.Float(default, 3.14)
                        )
                    ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }
}