using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class CommonFormatterTests
{
    [Test]
    public void FormatEmptyFunctionTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create("main", [], TypeNode.Create("void"), new BlockStatementNode())
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function main(): void {
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatTwoFunctionsWithParametersTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add", [
                    new FunctionParameterNode("x", TypeNode.Create("i32")),
                    new FunctionParameterNode("y", TypeNode.Create("i32")),
                ],
                TypeNode.Create("void"),
                new BlockStatementNode()),
            FunctionDeclarationNode.Create("main", [], TypeNode.Create("void"), new BlockStatementNode()),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32, y: i32): void {
            }

            function main(): void {
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatAdditionTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add", [
                    new FunctionParameterNode("x", TypeNode.Create("i32")),
                    new FunctionParameterNode("y", TypeNode.Create("i32")),
                ],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.Addition,
                            new MemberAccessExpressionNode("x"),
                            new MemberAccessExpressionNode("y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32, y: i32): void {
                return x + y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatSubtractionTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add", [
                    new FunctionParameterNode("x", TypeNode.Create("i32")),
                    new FunctionParameterNode("y", TypeNode.Create("i32")),
                ],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.Subtraction,
                            new MemberAccessExpressionNode("x"),
                            new MemberAccessExpressionNode("y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32, y: i32): void {
                return x - y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatMultiplicationTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add", [
                    new FunctionParameterNode("x", TypeNode.Create("i32")),
                    new FunctionParameterNode("y", TypeNode.Create("i32")),
                ],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.Multiplication,
                            new MemberAccessExpressionNode("x"),
                            new MemberAccessExpressionNode("y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32, y: i32): void {
                return x * y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatDivisionTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add", [
                    new FunctionParameterNode("x", TypeNode.Create("i32")),
                    new FunctionParameterNode("y", TypeNode.Create("i32")),
                ],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.Division,
                            new MemberAccessExpressionNode("x"),
                            new MemberAccessExpressionNode("y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32, y: i32): void {
                return x / y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatBitwiseAndTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add", [
                    new FunctionParameterNode("x", TypeNode.Create("i32")),
                    new FunctionParameterNode("y", TypeNode.Create("i32")),
                ],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.BitwiseAnd,
                            new MemberAccessExpressionNode("x"),
                            new MemberAccessExpressionNode("y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32, y: i32): void {
                return x & y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatBitwiseOrTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add", [
                    new FunctionParameterNode("x", TypeNode.Create("i32")),
                    new FunctionParameterNode("y", TypeNode.Create("i32")),
                ],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.BitwiseOr,
                            new MemberAccessExpressionNode("x"),
                            new MemberAccessExpressionNode("y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32, y: i32): void {
                return x | y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatBitwiseXorTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add", [
                    new FunctionParameterNode("x", TypeNode.Create("i32")),
                    new FunctionParameterNode("y", TypeNode.Create("i32")),
                ],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.BitwiseXor,
                            new MemberAccessExpressionNode("x"),
                            new MemberAccessExpressionNode("y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32, y: i32): void {
                return x ^ y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatConditionalAndTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add", [
                    new FunctionParameterNode("x", TypeNode.Create("i32")),
                    new FunctionParameterNode("y", TypeNode.Create("i32")),
                ],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.ConditionalAnd,
                            new MemberAccessExpressionNode("x"),
                            new MemberAccessExpressionNode("y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32, y: i32): void {
                return x && y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatConditionalOrTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add", [
                    new FunctionParameterNode("x", TypeNode.Create("i32")),
                    new FunctionParameterNode("y", TypeNode.Create("i32")),
                ],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.ConditionalOr,
                            new MemberAccessExpressionNode("x"),
                            new MemberAccessExpressionNode("y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32, y: i32): void {
                return x || y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatEqualityTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add", [
                    new FunctionParameterNode("x", TypeNode.Create("i32")),
                    new FunctionParameterNode("y", TypeNode.Create("i32")),
                ],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.Equality,
                            new MemberAccessExpressionNode("x"),
                            new MemberAccessExpressionNode("y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32, y: i32): void {
                return x == y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatInequalityTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add", [
                    new FunctionParameterNode("x", TypeNode.Create("i32")),
                    new FunctionParameterNode("y", TypeNode.Create("i32")),
                ],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.Inequality,
                            new MemberAccessExpressionNode("x"),
                            new MemberAccessExpressionNode("y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32, y: i32): void {
                return x != y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatLessThanTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add", [
                    new FunctionParameterNode("x", TypeNode.Create("i32")),
                    new FunctionParameterNode("y", TypeNode.Create("i32")),
                ],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.LessThan,
                            new MemberAccessExpressionNode("x"),
                            new MemberAccessExpressionNode("y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32, y: i32): void {
                return x < y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatLessThanOrEqualTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add", [
                    new FunctionParameterNode("x", TypeNode.Create("i32")),
                    new FunctionParameterNode("y", TypeNode.Create("i32")),
                ],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.LessThanOrEqual,
                            new MemberAccessExpressionNode("x"),
                            new MemberAccessExpressionNode("y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32, y: i32): void {
                return x <= y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatGreaterThanTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add", [
                    new FunctionParameterNode("x", TypeNode.Create("i32")),
                    new FunctionParameterNode("y", TypeNode.Create("i32")),
                ],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.GreaterThan,
                            new MemberAccessExpressionNode("x"),
                            new MemberAccessExpressionNode("y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32, y: i32): void {
                return x > y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatGreaterThanOrEqualTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add", [
                    new FunctionParameterNode("x", TypeNode.Create("i32")),
                    new FunctionParameterNode("y", TypeNode.Create("i32")),
                ],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.GreaterThanOrEqual,
                            new MemberAccessExpressionNode("x"),
                            new MemberAccessExpressionNode("y")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32, y: i32): void {
                return x >= y;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatUnaryPlusTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add",
                [new FunctionParameterNode("x", TypeNode.Create("i32"))],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new UnaryExpressionNode(
                            UnaryExpressionKind.UnaryPlus,
                            new MemberAccessExpressionNode("x")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32): void {
                return +x;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatUnaryMinusTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add",
                [new FunctionParameterNode("x", TypeNode.Create("i32"))],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new UnaryExpressionNode(
                            UnaryExpressionKind.UnaryMinus,
                            new MemberAccessExpressionNode("x")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32): void {
                return -x;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatLogicalNotTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add",
                [new FunctionParameterNode("x", TypeNode.Create("i32"))],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new UnaryExpressionNode(
                            UnaryExpressionKind.LogicalNot,
                            new MemberAccessExpressionNode("x")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32): void {
                return !x;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatBitwiseNotTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add",
                [new FunctionParameterNode("x", TypeNode.Create("i32"))],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new UnaryExpressionNode(
                            UnaryExpressionKind.BitwiseNot,
                            new MemberAccessExpressionNode("x")
                        )
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32): void {
                return ~x;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatIfTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add",
                [new FunctionParameterNode("x", TypeNode.Create("i32"))],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new IfStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.LessThan,
                            new MemberAccessExpressionNode("x"),
                            new LiteralExpressionNode(LiteralExpressionKind.Number, 0)
                        ),
                        new BlockStatementNode([
                            new ReturnStatementNode(
                                new UnaryExpressionNode(
                                    UnaryExpressionKind.UnaryMinus,
                                    new MemberAccessExpressionNode("x")
                                )
                            )
                        ])
                    ),
                    new ReturnStatementNode(
                        new MemberAccessExpressionNode("x")
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32): void {
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
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add",
                [new FunctionParameterNode("x", TypeNode.Create("i32"))],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new IfStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.LessThan,
                            new MemberAccessExpressionNode("x"),
                            new LiteralExpressionNode(LiteralExpressionKind.Number, 0)
                        ),
                        new BlockStatementNode([
                            new ReturnStatementNode(
                                new UnaryExpressionNode(
                                    UnaryExpressionKind.UnaryMinus,
                                    new MemberAccessExpressionNode("x")
                                )
                            )
                        ]),
                        new BlockStatementNode([
                            new ReturnStatementNode(
                                new MemberAccessExpressionNode("x")
                            )
                        ])
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32): void {
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
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add", [
                    new FunctionParameterNode("x", TypeNode.Create("i32")),
                    new FunctionParameterNode("y", TypeNode.Create("i32")),
                ],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.Addition,
                            new MemberAccessExpressionNode("x"),
                            new MemberAccessExpressionNode("y")
                        )
                    )
                ])),
            FunctionDeclarationNode.Create(
                "main",
                [],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new CallExpressionNode(
                            new MemberAccessExpressionNode("add"),
                            [
                                new LiteralExpressionNode(LiteralExpressionKind.Number, 1),
                                new LiteralExpressionNode(LiteralExpressionKind.Number, 2),
                            ])
                    )
                ])),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32, y: i32): void {
                return x + y;
            }

            function main(): void {
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
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add",
                [new FunctionParameterNode("x", TypeNode.Create("i32"))],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new ExpressionStatementNode(
                        new BinaryExpressionNode(
                            kind,
                            new MemberAccessExpressionNode("x"),
                            LiteralExpressionNode.Number(1))
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        var expected =
            $$"""
              function add(x: i32): void {
                  x {{@operator}} 1;
              }
              """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatWhileTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add",
                [new FunctionParameterNode("x", TypeNode.Create("i32"))],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new WhileNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.LessThan,
                            new MemberAccessExpressionNode("x"),
                            LiteralExpressionNode.Number(0)
                        ),
                        new BlockStatementNode()
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32): void {
                while (x < 0) {
                }
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatBreakTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add",
                [new FunctionParameterNode("x", TypeNode.Create("i32"))],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new WhileNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.LessThan,
                            new MemberAccessExpressionNode("x"),
                            LiteralExpressionNode.Number(0)
                        ),
                        new BlockStatementNode([
                            new BreakNode()
                        ])
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32): void {
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
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add",
                [new FunctionParameterNode("x", TypeNode.Create("i32"))],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new WhileNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.LessThan,
                            new MemberAccessExpressionNode("x"),
                            LiteralExpressionNode.Number(0)
                        ),
                        new BlockStatementNode([
                            new ContinueNode()
                        ])
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32): void {
                while (x < 0) {
                    continue;
                }
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void ArrayTypeTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add",
                [new FunctionParameterNode("x", TypeNode.Array("i32"))],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        LiteralExpressionNode.Number(0)
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32[]): void {
                return 0;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void ArrayAccessTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add",
                [new FunctionParameterNode("x", TypeNode.Array("i32"))],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new ArrayAccessExpressionNode(
                            new MemberAccessExpressionNode("x"),
                            LiteralExpressionNode.Number(0))
                    )
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function add(x: i32[]): void {
                return x[0];
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }
}