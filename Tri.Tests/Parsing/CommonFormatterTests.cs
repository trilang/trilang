using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class CommonFormatterTests
{
    [Test]
    public void FormatEmptyFunctionTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create("main", [], "void", new BlockStatementNode())
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
                    new FunctionParameterNode("x", "i32"),
                    new FunctionParameterNode("y", "i32"),
                ],
                "void",
                new BlockStatementNode()),
            FunctionDeclarationNode.Create("main", [], "void", new BlockStatementNode()),
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
                    new FunctionParameterNode("x", "i32"),
                    new FunctionParameterNode("y", "i32"),
                ],
                "void",
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.Addition,
                            new VariableExpressionNode("x"),
                            new VariableExpressionNode("y")
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
                    new FunctionParameterNode("x", "i32"),
                    new FunctionParameterNode("y", "i32"),
                ],
                "void",
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.Subtraction,
                            new VariableExpressionNode("x"),
                            new VariableExpressionNode("y")
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
                    new FunctionParameterNode("x", "i32"),
                    new FunctionParameterNode("y", "i32"),
                ],
                "void",
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.Multiplication,
                            new VariableExpressionNode("x"),
                            new VariableExpressionNode("y")
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
                    new FunctionParameterNode("x", "i32"),
                    new FunctionParameterNode("y", "i32"),
                ],
                "void",
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.Division,
                            new VariableExpressionNode("x"),
                            new VariableExpressionNode("y")
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
                    new FunctionParameterNode("x", "i32"),
                    new FunctionParameterNode("y", "i32"),
                ],
                "void",
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.BitwiseAnd,
                            new VariableExpressionNode("x"),
                            new VariableExpressionNode("y")
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
                    new FunctionParameterNode("x", "i32"),
                    new FunctionParameterNode("y", "i32"),
                ],
                "void",
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.BitwiseOr,
                            new VariableExpressionNode("x"),
                            new VariableExpressionNode("y")
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
                    new FunctionParameterNode("x", "i32"),
                    new FunctionParameterNode("y", "i32"),
                ],
                "void",
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.BitwiseXor,
                            new VariableExpressionNode("x"),
                            new VariableExpressionNode("y")
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
                    new FunctionParameterNode("x", "i32"),
                    new FunctionParameterNode("y", "i32"),
                ],
                "void",
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.ConditionalAnd,
                            new VariableExpressionNode("x"),
                            new VariableExpressionNode("y")
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
                    new FunctionParameterNode("x", "i32"),
                    new FunctionParameterNode("y", "i32"),
                ],
                "void",
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.ConditionalOr,
                            new VariableExpressionNode("x"),
                            new VariableExpressionNode("y")
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
                    new FunctionParameterNode("x", "i32"),
                    new FunctionParameterNode("y", "i32"),
                ],
                "void",
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.Equality,
                            new VariableExpressionNode("x"),
                            new VariableExpressionNode("y")
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
                    new FunctionParameterNode("x", "i32"),
                    new FunctionParameterNode("y", "i32"),
                ],
                "void",
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.Inequality,
                            new VariableExpressionNode("x"),
                            new VariableExpressionNode("y")
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
                    new FunctionParameterNode("x", "i32"),
                    new FunctionParameterNode("y", "i32"),
                ],
                "void",
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.LessThan,
                            new VariableExpressionNode("x"),
                            new VariableExpressionNode("y")
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
                    new FunctionParameterNode("x", "i32"),
                    new FunctionParameterNode("y", "i32"),
                ],
                "void",
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.LessThanOrEqual,
                            new VariableExpressionNode("x"),
                            new VariableExpressionNode("y")
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
                    new FunctionParameterNode("x", "i32"),
                    new FunctionParameterNode("y", "i32"),
                ],
                "void",
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.GreaterThan,
                            new VariableExpressionNode("x"),
                            new VariableExpressionNode("y")
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
                    new FunctionParameterNode("x", "i32"),
                    new FunctionParameterNode("y", "i32"),
                ],
                "void",
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.GreaterThanOrEqual,
                            new VariableExpressionNode("x"),
                            new VariableExpressionNode("y")
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
                [new FunctionParameterNode("x", "i32")],
                "void",
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new UnaryExpressionNode(
                            UnaryExpressionKind.UnaryPlus,
                            new VariableExpressionNode("x")
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
                [new FunctionParameterNode("x", "i32")],
                "void",
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new UnaryExpressionNode(
                            UnaryExpressionKind.UnaryMinus,
                            new VariableExpressionNode("x")
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
                [new FunctionParameterNode("x", "i32")],
                "void",
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new UnaryExpressionNode(
                            UnaryExpressionKind.LogicalNot,
                            new VariableExpressionNode("x")
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
                [new FunctionParameterNode("x", "i32")],
                "void",
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new UnaryExpressionNode(
                            UnaryExpressionKind.BitwiseNot,
                            new VariableExpressionNode("x")
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
                [new FunctionParameterNode("x", "i32")],
                "void",
                new BlockStatementNode([
                    new IfStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.LessThan,
                            new VariableExpressionNode("x"),
                            new LiteralExpressionNode(LiteralExpressionKind.Number, 0)
                        ),
                        new BlockStatementNode([
                            new ReturnStatementNode(
                                new UnaryExpressionNode(
                                    UnaryExpressionKind.UnaryMinus,
                                    new VariableExpressionNode("x")
                                )
                            )
                        ])
                    ),
                    new ReturnStatementNode(
                        new VariableExpressionNode("x")
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
                [new FunctionParameterNode("x", "i32")],
                "void",
                new BlockStatementNode([
                    new IfStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.LessThan,
                            new VariableExpressionNode("x"),
                            new LiteralExpressionNode(LiteralExpressionKind.Number, 0)
                        ),
                        new BlockStatementNode([
                            new ReturnStatementNode(
                                new UnaryExpressionNode(
                                    UnaryExpressionKind.UnaryMinus,
                                    new VariableExpressionNode("x")
                                )
                            )
                        ]),
                        new BlockStatementNode([
                            new ReturnStatementNode(
                                new VariableExpressionNode("x")
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
                    new FunctionParameterNode("x", "i32"),
                    new FunctionParameterNode("y", "i32"),
                ],
                "void",
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.Addition,
                            new VariableExpressionNode("x"),
                            new VariableExpressionNode("y")
                        )
                    )
                ])),
            FunctionDeclarationNode.Create(
                "main",
                [],
                "void",
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new CallExpressionNode("add", [
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
                [new FunctionParameterNode("x", "i32")],
                "void",
                new BlockStatementNode([
                    new ExpressionStatementNode(
                        new BinaryExpressionNode(
                            kind,
                            new VariableExpressionNode("x"),
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
}