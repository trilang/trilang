using Trilang;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class FormatterTests
{
    private static readonly SourceFile file = new SourceFile("test.tri");

    [Test]
    public void FormatEmptyFunctionTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "main",
                [],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default)
            )
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public main(): void {
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatTwoFunctionsWithParametersTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [
                    new ParameterNode(default, "x", new TypeRefNode(default, "i32")),
                    new ParameterNode(default, "y", new TypeRefNode(default, "i32")),
                ],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default)),
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "main",
                [],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default)
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32, y: i32): void {
            }

            public main(): void {
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatEmptyReturnTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "main",
                [],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new ReturnStatementNode(default)
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public main(): void {
                return;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatAdditionTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [
                    new ParameterNode(default, "x", new TypeRefNode(default, "i32")),
                    new ParameterNode(default, "y", new TypeRefNode(default, "i32")),
                ],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new ReturnStatementNode(
                        default,
                        new BinaryExpressionNode(
                            default,
                            BinaryExpressionKind.Addition,
                            new MemberAccessExpressionNode(default, "x"),
                            new MemberAccessExpressionNode(default, "y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32, y: i32): void {
                return x + y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatSubtractionTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [
                    new ParameterNode(default, "x", new TypeRefNode(default, "i32")),
                    new ParameterNode(default, "y", new TypeRefNode(default, "i32")),
                ],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new ReturnStatementNode(
                        default,
                        new BinaryExpressionNode(
                            default,
                            BinaryExpressionKind.Subtraction,
                            new MemberAccessExpressionNode(default, "x"),
                            new MemberAccessExpressionNode(default, "y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32, y: i32): void {
                return x - y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatMultiplicationTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [
                    new ParameterNode(default, "x", new TypeRefNode(default, "i32")),
                    new ParameterNode(default, "y", new TypeRefNode(default, "i32")),
                ],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new ReturnStatementNode(
                        default,
                        new BinaryExpressionNode(
                            default,
                            BinaryExpressionKind.Multiplication,
                            new MemberAccessExpressionNode(default, "x"),
                            new MemberAccessExpressionNode(default, "y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32, y: i32): void {
                return x * y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatDivisionTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [
                    new ParameterNode(default, "x", new TypeRefNode(default, "i32")),
                    new ParameterNode(default, "y", new TypeRefNode(default, "i32")),
                ],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new ReturnStatementNode(
                        default,
                        new BinaryExpressionNode(
                            default,
                            BinaryExpressionKind.Division,
                            new MemberAccessExpressionNode(default, "x"),
                            new MemberAccessExpressionNode(default, "y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32, y: i32): void {
                return x / y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatBitwiseAndTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [
                    new ParameterNode(default, "x", new TypeRefNode(default, "i32")),
                    new ParameterNode(default, "y", new TypeRefNode(default, "i32")),
                ],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new ReturnStatementNode(
                        default,
                        new BinaryExpressionNode(
                            default,
                            BinaryExpressionKind.BitwiseAnd,
                            new MemberAccessExpressionNode(default, "x"),
                            new MemberAccessExpressionNode(default, "y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32, y: i32): void {
                return x & y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatBitwiseOrTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [
                    new ParameterNode(default, "x", new TypeRefNode(default, "i32")),
                    new ParameterNode(default, "y", new TypeRefNode(default, "i32")),
                ],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new ReturnStatementNode(
                        default,
                        new BinaryExpressionNode(
                            default,
                            BinaryExpressionKind.BitwiseOr,
                            new MemberAccessExpressionNode(default, "x"),
                            new MemberAccessExpressionNode(default, "y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32, y: i32): void {
                return x | y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatBitwiseXorTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [
                    new ParameterNode(default, "x", new TypeRefNode(default, "i32")),
                    new ParameterNode(default, "y", new TypeRefNode(default, "i32")),
                ],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new ReturnStatementNode(
                        default,
                        new BinaryExpressionNode(
                            default,
                            BinaryExpressionKind.BitwiseXor,
                            new MemberAccessExpressionNode(default, "x"),
                            new MemberAccessExpressionNode(default, "y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32, y: i32): void {
                return x ^ y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatConditionalAndTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [
                    new ParameterNode(default, "x", new TypeRefNode(default, "i32")),
                    new ParameterNode(default, "y", new TypeRefNode(default, "i32")),
                ],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new ReturnStatementNode(
                        default,
                        new BinaryExpressionNode(
                            default,
                            BinaryExpressionKind.ConditionalAnd,
                            new MemberAccessExpressionNode(default, "x"),
                            new MemberAccessExpressionNode(default, "y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32, y: i32): void {
                return x && y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatConditionalOrTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [
                    new ParameterNode(default, "x", new TypeRefNode(default, "i32")),
                    new ParameterNode(default, "y", new TypeRefNode(default, "i32")),
                ],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new ReturnStatementNode(
                        default,
                        new BinaryExpressionNode(
                            default,
                            BinaryExpressionKind.ConditionalOr,
                            new MemberAccessExpressionNode(default, "x"),
                            new MemberAccessExpressionNode(default, "y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32, y: i32): void {
                return x || y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatEqualityTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [
                    new ParameterNode(default, "x", new TypeRefNode(default, "i32")),
                    new ParameterNode(default, "y", new TypeRefNode(default, "i32")),
                ],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new ReturnStatementNode(
                        default,
                        new BinaryExpressionNode(
                            default,
                            BinaryExpressionKind.Equality,
                            new MemberAccessExpressionNode(default, "x"),
                            new MemberAccessExpressionNode(default, "y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32, y: i32): void {
                return x == y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatInequalityTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [
                    new ParameterNode(default, "x", new TypeRefNode(default, "i32")),
                    new ParameterNode(default, "y", new TypeRefNode(default, "i32")),
                ],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new ReturnStatementNode(
                        default,
                        new BinaryExpressionNode(
                            default,
                            BinaryExpressionKind.Inequality,
                            new MemberAccessExpressionNode(default, "x"),
                            new MemberAccessExpressionNode(default, "y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32, y: i32): void {
                return x != y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatLessThanTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add", [
                    new ParameterNode(default, "x", new TypeRefNode(default, "i32")),
                    new ParameterNode(default, "y", new TypeRefNode(default, "i32")),
                ],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new ReturnStatementNode(
                        default,
                        new BinaryExpressionNode(
                            default,
                            BinaryExpressionKind.LessThan,
                            new MemberAccessExpressionNode(default, "x"),
                            new MemberAccessExpressionNode(default, "y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32, y: i32): void {
                return x < y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatLessThanOrEqualTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [
                    new ParameterNode(default, "x", new TypeRefNode(default, "i32")),
                    new ParameterNode(default, "y", new TypeRefNode(default, "i32")),
                ],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new ReturnStatementNode(
                        default,
                        new BinaryExpressionNode(
                            default,
                            BinaryExpressionKind.LessThanOrEqual,
                            new MemberAccessExpressionNode(default, "x"),
                            new MemberAccessExpressionNode(default, "y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32, y: i32): void {
                return x <= y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatGreaterThanTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [
                    new ParameterNode(default, "x", new TypeRefNode(default, "i32")),
                    new ParameterNode(default, "y", new TypeRefNode(default, "i32")),
                ],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new ReturnStatementNode(
                        default,
                        new BinaryExpressionNode(
                            default,
                            BinaryExpressionKind.GreaterThan,
                            new MemberAccessExpressionNode(default, "x"),
                            new MemberAccessExpressionNode(default, "y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32, y: i32): void {
                return x > y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatGreaterThanOrEqualTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [
                    new ParameterNode(default, "x", new TypeRefNode(default, "i32")),
                    new ParameterNode(default, "y", new TypeRefNode(default, "i32")),
                ],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new ReturnStatementNode(
                        default,
                        new BinaryExpressionNode(
                            default,
                            BinaryExpressionKind.GreaterThanOrEqual,
                            new MemberAccessExpressionNode(default, "x"),
                            new MemberAccessExpressionNode(default, "y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32, y: i32): void {
                return x >= y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatUnaryPlusTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [new ParameterNode(default, "x", new TypeRefNode(default, "i32"))],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new ReturnStatementNode(
                        default,
                        new UnaryExpressionNode(
                            default,
                            UnaryExpressionKind.UnaryPlus,
                            new MemberAccessExpressionNode(default, "x")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32): void {
                return +x;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatUnaryMinusTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [new ParameterNode(default, "x", new TypeRefNode(default, "i32"))],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new ReturnStatementNode(
                        default,
                        new UnaryExpressionNode(
                            default,
                            UnaryExpressionKind.UnaryMinus,
                            new MemberAccessExpressionNode(default, "x")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32): void {
                return -x;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatLogicalNotTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [new ParameterNode(default, "x", new TypeRefNode(default, "i32"))],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new ReturnStatementNode(
                        default,
                        new UnaryExpressionNode(
                            default,
                            UnaryExpressionKind.LogicalNot,
                            new MemberAccessExpressionNode(default, "x")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32): void {
                return !x;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatBitwiseNotTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [new ParameterNode(default, "x", new TypeRefNode(default, "i32"))],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new ReturnStatementNode(
                        default,
                        new UnaryExpressionNode(
                            default,
                            UnaryExpressionKind.BitwiseNot,
                            new MemberAccessExpressionNode(default, "x")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32): void {
                return ~x;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatIfTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [new ParameterNode(default, "x", new TypeRefNode(default, "i32"))],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new IfStatementNode(
                        default,
                        new BinaryExpressionNode(
                            default,
                            BinaryExpressionKind.LessThan,
                            new MemberAccessExpressionNode(default, "x"),
                            new LiteralExpressionNode(default, LiteralExpressionKind.Integer, 0)
                        ),
                        new BlockStatementNode(default, [
                            new ReturnStatementNode(
                                default,
                                new UnaryExpressionNode(
                                    default,
                                    UnaryExpressionKind.UnaryMinus,
                                    new MemberAccessExpressionNode(default, "x")
                                )
                            )
                        ])
                    ),
                    new ReturnStatementNode(
                        default,
                        new MemberAccessExpressionNode(default, "x")
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32): void {
                if (x < 0) {
                    return -x;
                }
                return x;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatIfElseTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [new ParameterNode(default, "x", new TypeRefNode(default, "i32"))],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new IfStatementNode(
                        default,
                        new BinaryExpressionNode(
                            default,
                            BinaryExpressionKind.LessThan,
                            new MemberAccessExpressionNode(default, "x"),
                            new LiteralExpressionNode(default, LiteralExpressionKind.Integer, 0)
                        ),
                        new BlockStatementNode(default, [
                            new ReturnStatementNode(
                                default,
                                new UnaryExpressionNode(
                                    default,
                                    UnaryExpressionKind.UnaryMinus,
                                    new MemberAccessExpressionNode(default, "x")
                                )
                            )
                        ]),
                        new BlockStatementNode(default, [
                            new ReturnStatementNode(
                                default,
                                new MemberAccessExpressionNode(default, "x")
                            )
                        ])
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32): void {
                if (x < 0) {
                    return -x;
                } else {
                    return x;
                }
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatFunctionCallTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [
                    new ParameterNode(default, "x", new TypeRefNode(default, "i32")),
                    new ParameterNode(default, "y", new TypeRefNode(default, "i32")),
                ],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new ReturnStatementNode(
                        default,
                        new BinaryExpressionNode(
                            default,
                            BinaryExpressionKind.Addition,
                            new MemberAccessExpressionNode(default, "x"),
                            new MemberAccessExpressionNode(default, "y")
                        )
                    )
                ])),
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
                            new MemberAccessExpressionNode(default, "add"),
                            [
                                new LiteralExpressionNode(default, LiteralExpressionKind.Integer, 1),
                                new LiteralExpressionNode(default, LiteralExpressionKind.Integer, 2),
                            ]
                        )
                    )
                ])),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32, y: i32): void {
                return x + y;
            }

            public main(): void {
                return add(1, 2);
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    [TestCase(BinaryExpressionKind.Assignment, "=")]
    [TestCase(BinaryExpressionKind.AdditionAssignment, "+=")]
    [TestCase(BinaryExpressionKind.SubtractionAssignment, "-=")]
    [TestCase(BinaryExpressionKind.MultiplicationAssignment, "*=")]
    [TestCase(BinaryExpressionKind.DivisionAssignment, "/=")]
    [TestCase(BinaryExpressionKind.ModulusAssignment, "%=")]
    [TestCase(BinaryExpressionKind.BitwiseAndAssignment, "&=")]
    [TestCase(BinaryExpressionKind.BitwiseOrAssignment, "|=")]
    [TestCase(BinaryExpressionKind.BitwiseXorAssignment, "^=")]
    public void FormatAssignmentTest(BinaryExpressionKind kind, string @operator)
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [new ParameterNode(default, "x", new TypeRefNode(default, "i32"))],
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
            ),
        ]);
        var formatted = tree.ToString();
        var expected =
            $$"""
              public add(x: i32): void {
                  x {{@operator}} 1;
              }
              """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatWhileTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [new ParameterNode(default, "x", new TypeRefNode(default, "i32"))],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new WhileNode(
                        default,
                        new BinaryExpressionNode(
                            default,
                            BinaryExpressionKind.LessThan,
                            new MemberAccessExpressionNode(default, "x"),
                            LiteralExpressionNode.Integer(default, 0)
                        ),
                        new BlockStatementNode(default)
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32): void {
                while (x < 0) {
                }
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatBreakTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [new ParameterNode(default, "x", new TypeRefNode(default, "i32"))],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new WhileNode(
                        default,
                        new BinaryExpressionNode(
                            default,
                            BinaryExpressionKind.LessThan,
                            new MemberAccessExpressionNode(default, "x"),
                            LiteralExpressionNode.Integer(default, 0)
                        ),
                        new BlockStatementNode(default, [
                            new BreakNode(default)
                        ])
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32): void {
                while (x < 0) {
                    break;
                }
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatContinueTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [new ParameterNode(default, "x", new TypeRefNode(default, "i32"))],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new WhileNode(
                        default,
                        new BinaryExpressionNode(
                            default,
                            BinaryExpressionKind.LessThan,
                            new MemberAccessExpressionNode(default, "x"),
                            LiteralExpressionNode.Integer(default, 0)
                        ),
                        new BlockStatementNode(default, [
                            new ContinueNode(default)
                        ])
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32): void {
                while (x < 0) {
                    continue;
                }
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatArrayTypeTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [new ParameterNode(default, "x", new ArrayTypeNode(default, new TypeRefNode(default, "i32")))],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new ReturnStatementNode(
                        default,
                        LiteralExpressionNode.Integer(default, 0)
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32[]): void {
                return 0;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatArrayAccessTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "add",
                [new ParameterNode(default, "x", new ArrayTypeNode(default, new TypeRefNode(default, "i32")))],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new ReturnStatementNode(
                        default,
                        new ArrayAccessExpressionNode(
                            default,
                            new MemberAccessExpressionNode(default, "x"),
                            LiteralExpressionNode.Integer(default, 0)
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public add(x: i32[]): void {
                return x[0];
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatPrivateTypeTest()
    {
        var tree = new SyntaxTree(file, [
            new TypeDeclarationNode(default, AccessModifier.Private, "MyType", [], [], [], [], [])
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            private type MyType {
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatPublicTypeTest()
    {
        var tree = new SyntaxTree(file, [
            new TypeDeclarationNode(default, AccessModifier.Public, "MyType", [], [], [], [], [])
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public type MyType {
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatTwoTypesTest()
    {
        var tree = new SyntaxTree(file, [
            new TypeDeclarationNode(default, AccessModifier.Private, "MyType1", [], [], [], [], []),
            new TypeDeclarationNode(default, AccessModifier.Public, "MyType2", [], [], [], [], []),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            private type MyType1 {
            }

            public type MyType2 {
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatPointTypeWithPropertiesTest()
    {
        var tree = new SyntaxTree(file, [
            new TypeDeclarationNode(
                default,
                AccessModifier.Public,
                "Point",
                [],
                [],
                [
                    new PropertyDeclarationNode(default, "x", new TypeRefNode(default, "i32")),
                    new PropertyDeclarationNode(default, "y", new TypeRefNode(default, "i32")),
                ],
                [],
                []
            )
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public type Point {
                x: i32;
                y: i32;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatPropertyWithGetterSetterTest()
    {
        var tree = new SyntaxTree(file, [
            new TypeDeclarationNode(
                default,
                AccessModifier.Public,
                "Point",
                [],
                [],
                [
                    new PropertyDeclarationNode(
                        default,
                        "x",
                        new TypeRefNode(default, "i32"),
                        new PropertyGetterNode(
                            default,
                            AccessModifier.Private,
                            new BlockStatementNode(default, [
                                new ReturnStatementNode(default, LiteralExpressionNode.Integer(default, 0))
                            ])
                        ),
                        new PropertySetterNode(
                            default,
                            AccessModifier.Private,
                            new BlockStatementNode(default, [
                                new ExpressionStatementNode(
                                    default,
                                    new BinaryExpressionNode(
                                        default,
                                        BinaryExpressionKind.Assignment,
                                        new MemberAccessExpressionNode(default, "field"),
                                        new MemberAccessExpressionNode(default, "value")
                                    )
                                )
                            ])
                        )),
                ],
                [],
                []
            )
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public type Point {
                x: i32 {
                    private get {
                        return 0;
                    }
                    private set {
                        field = value;
                    }
                }
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatPropertyWithEmptyGetterTest()
    {
        var tree = new SyntaxTree(file, [
            new TypeDeclarationNode(
                default,
                AccessModifier.Public,
                "Point",
                [],
                [],
                [
                    new PropertyDeclarationNode(
                        default,
                        "x",
                        new TypeRefNode(default, "i32"),
                        new PropertyGetterNode(default, AccessModifier.Private, null),
                        new PropertySetterNode(
                            default,
                            AccessModifier.Private,
                            new BlockStatementNode(default, [
                                new ExpressionStatementNode(
                                    default,
                                    new BinaryExpressionNode(
                                        default,
                                        BinaryExpressionKind.Assignment,
                                        new MemberAccessExpressionNode(default, "field"),
                                        new MemberAccessExpressionNode(default, "value")
                                    )
                                )
                            ])
                        )
                    ),
                ],
                [],
                []
            )
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public type Point {
                x: i32 {
                    private get;
                    private set {
                        field = value;
                    }
                }
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatPropertyWithEmptySetterTest()
    {
        var tree = new SyntaxTree(file, [
            new TypeDeclarationNode(
                default,
                AccessModifier.Public,
                "Point",
                [],
                [],
                [
                    new PropertyDeclarationNode(
                        default,
                        "x",
                        new TypeRefNode(default, "i32"),
                        new PropertyGetterNode(
                            default,
                            AccessModifier.Private,
                            new BlockStatementNode(default, [
                                new ReturnStatementNode(default, LiteralExpressionNode.Integer(default, 0))
                            ])
                        ),
                        new PropertySetterNode(default, AccessModifier.Private, null)),
                ],
                [],
                []
            )
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public type Point {
                x: i32 {
                    private get {
                        return 0;
                    }
                    private set;
                }
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatPointTypeWithEverythingTest()
    {
        var tree = new SyntaxTree(file, [
            new TypeDeclarationNode(
                default,
                AccessModifier.Public,
                "Point",
                [],
                [],
                [
                    new PropertyDeclarationNode(default, "x", new TypeRefNode(default, "i32")),
                    new PropertyDeclarationNode(default, "y", new TypeRefNode(default, "i32")),
                ],
                [
                    new ConstructorDeclarationNode(
                        default,
                        AccessModifier.Public,
                        [
                            new ParameterNode(default, "x", new TypeRefNode(default, "i32")),
                            new ParameterNode(default, "y", new TypeRefNode(default, "i32")),
                        ],
                        new BlockStatementNode(default))
                ],
                [
                    new MethodDeclarationNode(
                        default,
                        AccessModifier.Public,
                        false,
                        "toString",
                        [],
                        new TypeRefNode(default, "string"),
                        new BlockStatementNode(default)),
                    new MethodDeclarationNode(
                        default,
                        AccessModifier.Public,
                        false,
                        "distance",
                        [new ParameterNode(default, "other", new TypeRefNode(default, "Point"))],
                        new TypeRefNode(default, "string"),
                        new BlockStatementNode(default)),
                ])
        ]);
        var formatted = tree.ToString();
        const string expected =
            "public type Point {\n    x: i32;\n    y: i32;\n    public constructor(x: i32, y: i32) {\n    }\n    \n    public toString(): string {\n    }\n    \n    public distance(other: Point): string {\n    }\n}";

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatTypeWithInterfacesTest()
    {
        var tree = new SyntaxTree(file, [
            new TypeDeclarationNode(
                default,
                AccessModifier.Public,
                "MyType",
                [],
                [new TypeRefNode(default, "Interface1"), new TypeRefNode(default, "Interface2")],
                [],
                [],
                []
            )
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public type MyType : Interface1, Interface2 {
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatTypeAliasTest()
    {
        var tree = new SyntaxTree(file, [
            new AliasDeclarationNode(default, AccessModifier.Public, "MyType", [], new TypeRefNode(default, "i32"))
        ]);
        var formatted = tree.ToString();
        const string expected = "public type MyType = i32;";

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatFunctionTypeTest()
    {
        var tree = new SyntaxTree(file, [
            new AliasDeclarationNode(
                default,
                AccessModifier.Public,
                "MyType",
                [],
                new FunctionTypeNode(
                    default,
                    [new TypeRefNode(default, "i32"), new TypeRefNode(default, "i32")],
                    new TypeRefNode(default, "i32")
                )
            )
        ]);
        var formatted = tree.ToString();
        const string expected = "public type MyType = (i32, i32) => i32;";

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatVariableDeclarationTest()
    {
        var tree = new SyntaxTree(file, [
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
                        LiteralExpressionNode.Integer(default, 0)
                    )
                ]))
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public main(): void {
                var x: i32 = 0;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatVariableDeclarationWithInlineTypeTest()
    {
        var tree = new SyntaxTree(file, [
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
                        new FunctionTypeNode(default, [], new TypeRefNode(default, "void")),
                        LiteralExpressionNode.Integer(default, 0)
                    )
                ]))
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public main(): void {
                var x: () => void = 0;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatVariableDeclarationWithInlineTypeAndParametersTest()
    {
        var tree = new SyntaxTree(file, [
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
                        new FunctionTypeNode(
                            default,
                            [new TypeRefNode(default, "i32"), new TypeRefNode(default, "f64")],
                            new TypeRefNode(default, "void")
                        ),
                        LiteralExpressionNode.Integer(default, 0)
                    )
                ]))
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public main(): void {
                var x: (i32, f64) => void = 0;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatAliasInterfaceTypeTest()
    {
        var tree = new SyntaxTree(file, [
            new AliasDeclarationNode(
                default,
                AccessModifier.Public,
                "Point",
                [],
                new InterfaceNode(
                    default,
                    [
                        new InterfacePropertyNode(default, "x", new TypeRefNode(default, "i32"), null, null),
                        new InterfacePropertyNode(default, "y", new TypeRefNode(default, "i32"), null, null),
                    ],
                    [
                        new InterfaceMethodNode(
                            default,
                            "distance",
                            [new TypeRefNode(default, "Point")],
                            new TypeRefNode(default, "f64")
                        )
                    ]
                )
            )
        ]);
        var formatted = tree.ToString();
        const string expected = "public type Point = {\n    x: i32;\n    y: i32;\n    \n    distance(Point): f64;\n}";

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatAliasInterfaceTypeWithGettersSettersTest()
    {
        var tree = new SyntaxTree(file, [
            new AliasDeclarationNode(
                default,
                AccessModifier.Public,
                "Point",
                [],
                new InterfaceNode(
                    default,
                    [
                        new InterfacePropertyNode(default, "x", new TypeRefNode(default, "i32"), AccessModifier.Public, AccessModifier.Public),
                        new InterfacePropertyNode(default, "y", new TypeRefNode(default, "i32"), AccessModifier.Private, AccessModifier.Private),
                    ],
                    [
                        new InterfaceMethodNode(
                            default,
                            "distance",
                            [new TypeRefNode(default, "Point")],
                            new TypeRefNode(default, "f64")
                        )
                    ]
                )
            )
        ]);
        var formatted = tree.ToString();
        const string expected = "public type Point = {\n    x: i32 { public get; public set; }\n    y: i32 { private get; private set; }\n    \n    distance(Point): f64;\n}";

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatInlineInterfaceTypeTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "main",
                [],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new VariableDeclarationNode(
                        default,
                        "p",
                        new InterfaceNode(
                            default,
                            [
                                new InterfacePropertyNode(default, "x", new TypeRefNode(default, "i32"), null, null),
                                new InterfacePropertyNode(default, "y", new TypeRefNode(default, "i32"), null, null),
                            ],
                            []
                        ),
                        LiteralExpressionNode.Integer(default, 0)
                    )
                ])
            )
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public main(): void {
                var p: { x: i32; y: i32; } = 0;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatMultipleMemberAccessTest()
    {
        var tree = new SyntaxTree(file, [
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
        var formatted = tree.ToString();
        var expected =
            """
            public main(): void {
                return a.b.c;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatThisExpressionTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "main",
                [],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new ReturnStatementNode(
                        default,
                        new MemberAccessExpressionNode(default, "this")
                    )
                ])
            )
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public main(): void {
                return this;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatThisExpressionWithMemberAccessTest()
    {
        var tree = new SyntaxTree(file, [
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
                                new MemberAccessExpressionNode(default, "this"),
                                "a"
                            ),
                            "b"
                        )
                    )
                ])
            )
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public main(): void {
                return this.a.b;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatNewOperatorTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "main",
                [],
                new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new VariableDeclarationNode(
                        default,
                        "p",
                        new TypeRefNode(default, "Point"),
                        new NewObjectExpressionNode(
                            default,
                            new TypeRefNode(default, "Point"),
                            [LiteralExpressionNode.Integer(default, 1), LiteralExpressionNode.Integer(default, 2)]
                        )
                    )
                ])
            )
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public main(): void {
                var p: Point = new Point(1, 2);
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatDiscriminatedUnionOfTypesTest()
    {
        var tree = new SyntaxTree(file, [
            new AliasDeclarationNode(
                default,
                AccessModifier.Public,
                "Numbers",
                [],
                new DiscriminatedUnionNode([
                    new TypeRefNode(default, "i8"),
                    new TypeRefNode(default, "i16"),
                    new TypeRefNode(default, "i32"),
                    new TypeRefNode(default, "i64"),
                ]))
        ]);
        var formatted = tree.ToString();
        const string expected = "public type Numbers = i8 | i16 | i32 | i64;";

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatDiscriminatedUnionOfFunctionsTest()
    {
        var tree = new SyntaxTree(file, [
            new AliasDeclarationNode(
                default,
                AccessModifier.Public,
                "F",
                [],
                new DiscriminatedUnionNode([
                    new FunctionTypeNode(default, [], new TypeRefNode(default, "void")),
                    new FunctionTypeNode(default, [new TypeRefNode(default, "i32"), new TypeRefNode(default, "i32")], new TypeRefNode(default, "i32"))
                ]))
        ]);
        var formatted = tree.ToString();
        const string expected = "public type F = () => void | (i32, i32) => i32;";

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatDiscriminatedUnionOfInterfacesTest()
    {
        var tree = new SyntaxTree(file, [
            new AliasDeclarationNode(
                default,
                AccessModifier.Public,
                "I",
                [],
                new DiscriminatedUnionNode([
                    new InterfaceNode(default, [], []),
                    new InterfaceNode(
                        default,
                        [
                            new InterfacePropertyNode(default, "x", new TypeRefNode(default, "i32"), null, null),
                            new InterfacePropertyNode(default, "y", new TypeRefNode(default, "i32"), null, null),
                        ],
                        []
                    ),
                ]))
        ]);
        var formatted = tree.ToString();
        const string expected = "public type I = { } | { x: i32; y: i32; };";

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatDiscriminatedUnionTest()
    {
        var tree = new SyntaxTree(file, [
            new AliasDeclarationNode(
                default,
                AccessModifier.Public,
                "T",
                [],
                new DiscriminatedUnionNode([
                    new InterfaceNode(default, [], []),
                    new TypeRefNode(default, "i32"),
                    new FunctionTypeNode(default, [], new TypeRefNode(default, "void")),
                ]))
        ]);
        var formatted = tree.ToString();
        const string expected = "public type T = { } | i32 | () => void;";

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatNullTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(default, AccessModifier.Public, "main", [], new TypeRefNode(default, "void"),
                new BlockStatementNode(default, [
                    new VariableDeclarationNode(
                        default,
                        "x",
                        new DiscriminatedUnionNode([new TypeRefNode(default, "i32"), new TypeRefNode(default, "null")]),
                        new NullExpressionNode(default)
                    )
                ]))
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public main(): void {
                var x: i32 | null = null;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatTupleTypeTest()
    {
        var tree = new SyntaxTree(file, [
            new AliasDeclarationNode(
                default,
                AccessModifier.Public,
                "T",
                [],
                new TupleTypeNode(default, [
                    new TypeRefNode(default, "bool"),
                    new TypeRefNode(default, "i32"),
                ]))
        ]);
        var formatted = tree.ToString();
        const string expected = "public type T = (bool, i32);";

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatNestedTupleTypeTest()
    {
        var tree = new SyntaxTree(file, [
            new AliasDeclarationNode(
                default,
                AccessModifier.Public,
                "T",
                [],
                new TupleTypeNode(default, [
                    new TupleTypeNode(default, [new TypeRefNode(default, "bool"), new TypeRefNode(default, "i8")]),
                    new TypeRefNode(default, "i32"),
                ]))
        ]);
        var formatted = tree.ToString();
        const string expected = "public type T = ((bool, i8), i32);";

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatTupleExpressionTest()
    {
        var tree = new SyntaxTree(file, [
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
                            LiteralExpressionNode.True(default),
                            LiteralExpressionNode.Integer(default, 1)
                        ])
                    )
                ]))
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public main(): void {
                return (true, 1);
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatNewArrayTest()
    {
        var tree = new SyntaxTree(file, [
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
                            new ArrayTypeNode(default, new TypeRefNode(default, "i32")),
                            LiteralExpressionNode.Integer(default, 10)
                        )
                    )
                ])
            )
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public main(): void {
                return new i32[10];
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatGenericTypeTest()
    {
        var tree = new SyntaxTree(file, [
            new TypeDeclarationNode(
                default,
                AccessModifier.Public,
                "List",
                [new TypeRefNode(default, "T1, T2")],
                [],
                [],
                [],
                []
            )
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public type List<T1, T2> {
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatGenericTypeNodeTest()
    {
        var tree = new SyntaxTree(file, [
            new AliasDeclarationNode(
                default,
                AccessModifier.Public,
                "Test",
                [],
                new GenericTypeRefNode(default, "List", [new TypeRefNode(default, "T1"), new TypeRefNode(default, "T2")])
            )
        ]);
        var formatted = tree.ToString();
        const string expected = "public type Test = List<T1, T2>;";

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatGenericTypeAliasTest()
    {
        var tree = new SyntaxTree(file, [
            new AliasDeclarationNode(
                default,
                AccessModifier.Public,
                "T",
                [new TypeRefNode(default, "T1"), new TypeRefNode(default, "T2")],
                new DiscriminatedUnionNode([new TypeRefNode(default, "T1"), new TypeRefNode(default, "T2")])
            )
        ]);
        var formatted = tree.ToString();
        const string expected = "public type T<T1, T2> = T1 | T2;";

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatStaticMethodTest()
    {
        var tree = new SyntaxTree(file, [
            new TypeDeclarationNode(
                default,
                AccessModifier.Public,
                "Test",
                [],
                [],
                [],
                [],
                [
                    new MethodDeclarationNode(
                        default,
                        AccessModifier.Public,
                        true,
                        "method",
                        [],
                        new TypeRefNode(default, "void"),
                        new BlockStatementNode(default)
                    )
                ]
            )
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public type Test {
                public static method(): void {
                }
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatIfDirectiveTest()
    {
        var tree = new SyntaxTree(file, [
            new TypeDeclarationNode(default, AccessModifier.Public, "Type1", [], [], [], [], []),
            new IfDirectiveNode(
                default,
                "DEBUG",
                [
                    new TypeDeclarationNode(default, AccessModifier.Public, "Type2", [], [], [], [], []),
                    new TypeDeclarationNode(default, AccessModifier.Public, "Type3", [], [], [], [], []),
                ],
                []),
            new TypeDeclarationNode(default, AccessModifier.Public, "Type4", [], [], [], [], []),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public type Type1 {
            }

            #if DEBUG

            public type Type2 {
            }

            public type Type3 {
            }

            #endif

            public type Type4 {
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatIfDirectiveWithElseTest()
    {
        var tree = new SyntaxTree(file, [
            new TypeDeclarationNode(default, AccessModifier.Public, "Type1", [], [], [], [], []),
            new IfDirectiveNode(
                default,
                "DEBUG",
                [
                    new TypeDeclarationNode(default, AccessModifier.Public, "Type2", [], [], [], [], []),
                    new TypeDeclarationNode(default, AccessModifier.Public, "Type3", [], [], [], [], []),
                ],
                [
                    new TypeDeclarationNode(default, AccessModifier.Public, "Type5", [], [], [], [], []),
                    new TypeDeclarationNode(default, AccessModifier.Public, "Type6", [], [], [], [], []),
                ]),
            new TypeDeclarationNode(default, AccessModifier.Public, "Type4", [], [], [], [], []),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public type Type1 {
            }

            #if DEBUG

            public type Type2 {
            }

            public type Type3 {
            }

            #else

            public type Type5 {
            }

            public type Type6 {
            }

            #endif

            public type Type4 {
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatAsExpressionTest()
    {
        var tree = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                default,
                AccessModifier.Public,
                "test",
                [new ParameterNode(default, "a", new TypeRefNode(default, "i32"))],
                new TypeRefNode(default, "bool"),
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
        var formatted = tree.ToString();
        const string expected =
            """
            public test(a: i32): bool {
                return a is i8;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatCastExpressionTest()
    {
        var tree = new SyntaxTree(file, [
            new FunctionDeclarationNode(
                default,
                AccessModifier.Public,
                "test",
                [new ParameterNode(default, "a", new TypeRefNode(default, "i32"))],
                new TypeRefNode(default, "i8"),
                new BlockStatementNode(default, [
                    new ReturnStatementNode(
                        default,
                        new CastExpressionNode(
                            default,
                            new TypeRefNode(default, "i8"),
                            new MemberAccessExpressionNode(default, "a")
                        )
                    )
                ])
            )
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public test(a: i32): i8 {
                return (i8)a;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatFloatingNumberTest()
    {
        var tree = new SyntaxTree(file, [
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
        var formatted = tree.ToString();
        const string expected =
            """
            public test(): f64 {
                return 3.14;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatUseTest()
    {
        var tree = new SyntaxTree(
            file,
            [
                new UseNode(default, ["Test", "SubNamespace", "SubSubNamespace"])
            ],
            new NamespaceNode(default, ["Test", "Test2"]),
            [
                new FunctionDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(default, []))
            ]);
        var formatted = tree.ToString();
        const string expected =
            """
            use Test.SubNamespace.SubSubNamespace;

            namespace Test.Test2;

            public main(): void {
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }
}