using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class FormatterTests
{
    [Test]
    public void FormatEmptyFunctionTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create("main", [], new TypeNode("void"), new BlockStatementNode())
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
                    new ParameterNode("x", new TypeNode("i32")),
                    new ParameterNode("y", new TypeNode("i32")),
                ],
                new TypeNode("void"),
                new BlockStatementNode()),
            FunctionDeclarationNode.Create("main", [], new TypeNode("void"), new BlockStatementNode()),
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
    public void FormatEmptyReturnTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new ReturnStatementNode()
                ])
            ),
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function main(): void {
                return;
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
                    new ParameterNode("x", new TypeNode("i32")),
                    new ParameterNode("y", new TypeNode("i32")),
                ],
                new TypeNode("void"),
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
                    new ParameterNode("x", new TypeNode("i32")),
                    new ParameterNode("y", new TypeNode("i32")),
                ],
                new TypeNode("void"),
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
                    new ParameterNode("x", new TypeNode("i32")),
                    new ParameterNode("y", new TypeNode("i32")),
                ],
                new TypeNode("void"),
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
                    new ParameterNode("x", new TypeNode("i32")),
                    new ParameterNode("y", new TypeNode("i32")),
                ],
                new TypeNode("void"),
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
                    new ParameterNode("x", new TypeNode("i32")),
                    new ParameterNode("y", new TypeNode("i32")),
                ],
                new TypeNode("void"),
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
                    new ParameterNode("x", new TypeNode("i32")),
                    new ParameterNode("y", new TypeNode("i32")),
                ],
                new TypeNode("void"),
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
                    new ParameterNode("x", new TypeNode("i32")),
                    new ParameterNode("y", new TypeNode("i32")),
                ],
                new TypeNode("void"),
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
                    new ParameterNode("x", new TypeNode("i32")),
                    new ParameterNode("y", new TypeNode("i32")),
                ],
                new TypeNode("void"),
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
                    new ParameterNode("x", new TypeNode("i32")),
                    new ParameterNode("y", new TypeNode("i32")),
                ],
                new TypeNode("void"),
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
                    new ParameterNode("x", new TypeNode("i32")),
                    new ParameterNode("y", new TypeNode("i32")),
                ],
                new TypeNode("void"),
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
                    new ParameterNode("x", new TypeNode("i32")),
                    new ParameterNode("y", new TypeNode("i32")),
                ],
                new TypeNode("void"),
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
                    new ParameterNode("x", new TypeNode("i32")),
                    new ParameterNode("y", new TypeNode("i32")),
                ],
                new TypeNode("void"),
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
                    new ParameterNode("x", new TypeNode("i32")),
                    new ParameterNode("y", new TypeNode("i32")),
                ],
                new TypeNode("void"),
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
                    new ParameterNode("x", new TypeNode("i32")),
                    new ParameterNode("y", new TypeNode("i32")),
                ],
                new TypeNode("void"),
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
                    new ParameterNode("x", new TypeNode("i32")),
                    new ParameterNode("y", new TypeNode("i32")),
                ],
                new TypeNode("void"),
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
                [new ParameterNode("x", new TypeNode("i32"))],
                new TypeNode("void"),
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
                [new ParameterNode("x", new TypeNode("i32"))],
                new TypeNode("void"),
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
                [new ParameterNode("x", new TypeNode("i32"))],
                new TypeNode("void"),
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
                [new ParameterNode("x", new TypeNode("i32"))],
                new TypeNode("void"),
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
                [new ParameterNode("x", new TypeNode("i32"))],
                new TypeNode("void"),
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
                [new ParameterNode("x", new TypeNode("i32"))],
                new TypeNode("void"),
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
                    new ParameterNode("x", new TypeNode("i32")),
                    new ParameterNode("y", new TypeNode("i32")),
                ],
                new TypeNode("void"),
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
                new TypeNode("void"),
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
                [new ParameterNode("x", new TypeNode("i32"))],
                new TypeNode("void"),
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
                [new ParameterNode("x", new TypeNode("i32"))],
                new TypeNode("void"),
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
                [new ParameterNode("x", new TypeNode("i32"))],
                new TypeNode("void"),
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
                [new ParameterNode("x", new TypeNode("i32"))],
                new TypeNode("void"),
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
    public void FormatArrayTypeTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add",
                [new ParameterNode("x", new TypeNode("i32[]"))],
                new TypeNode("void"),
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
    public void FormatArrayAccessTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add",
                [new ParameterNode("x", new TypeNode("i32[]"))],
                new TypeNode("void"),
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

    [Test]
    public void FormatPrivateTypeTest()
    {
        var tree = new SyntaxTree([
            new TypeDeclarationNode(AccessModifier.Private, "MyType", [], [], [])
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
        var tree = new SyntaxTree([
            new TypeDeclarationNode(AccessModifier.Public, "MyType", [], [], [])
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
        var tree = new SyntaxTree([
            new TypeDeclarationNode(AccessModifier.Private, "MyType1", [], [], []),
            new TypeDeclarationNode(AccessModifier.Public, "MyType2", [], [], []),
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
    public void FormatPointTypeWithFieldsTest()
    {
        var tree = new SyntaxTree([
            new TypeDeclarationNode(
                AccessModifier.Public,
                "Point",
                [
                    new FieldDeclarationNode(AccessModifier.Private, "x", new TypeNode("i32")),
                    new FieldDeclarationNode(AccessModifier.Private, "y", new TypeNode("i32")),
                ],
                [],
                [])
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public type Point {
                private x: i32;
                private y: i32;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatPointTypeWithEverythingTest()
    {
        var tree = new SyntaxTree([
            new TypeDeclarationNode(
                AccessModifier.Public,
                "Point",
                [
                    new FieldDeclarationNode(AccessModifier.Private, "x", new TypeNode("i32")),
                    new FieldDeclarationNode(AccessModifier.Private, "y", new TypeNode("i32")),
                ],
                [
                    new ConstructorDeclarationNode(
                        AccessModifier.Public,
                        [
                            new ParameterNode("x", new TypeNode("i32")),
                            new ParameterNode("y", new TypeNode("i32")),
                        ],
                        new BlockStatementNode())
                ],
                [
                    new MethodDeclarationNode(
                        AccessModifier.Public,
                        "toString",
                        [],
                        new TypeNode("string"),
                        new BlockStatementNode()),
                    new MethodDeclarationNode(
                        AccessModifier.Public,
                        "distance",
                        [new ParameterNode("other", new TypeNode("Point"))],
                        new TypeNode("string"),
                        new BlockStatementNode()),
                ])
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            public type Point {
                private x: i32;
                private y: i32;
                public constructor(x: i32, y: i32) {
                }
                public toString(): string {
                }
                public distance(other: Point): string {
                }
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatTypeAliasTest()
    {
        var tree = new SyntaxTree([
            new TypeAliasDeclarationNode(AccessModifier.Public, "MyType", new TypeNode("i32"))
        ]);
        var formatted = tree.ToString();
        const string expected = "public type MyType = i32;";

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatFunctionTypeTest()
    {
        var tree = new SyntaxTree([
            new TypeAliasDeclarationNode(
                AccessModifier.Public,
                "MyType",
                new FunctionTypeNode(
                    [new TypeNode("i32"), new TypeNode("i32")],
                    new TypeNode("i32")
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
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new VariableDeclarationStatementNode(
                        "x",
                        new TypeNode("i32"),
                        LiteralExpressionNode.Number(0)
                    )
                ]))
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function main(): void {
                var x: i32 = 0;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatVariableDeclarationWithInlineTypeTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new VariableDeclarationStatementNode(
                        "x",
                        new FunctionTypeNode([], new TypeNode("void")),
                        LiteralExpressionNode.Number(0)
                    )
                ]))
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function main(): void {
                var x: () => void = 0;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatVariableDeclarationWithInlineTypeAndParametersTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new VariableDeclarationStatementNode(
                        "x",
                        new FunctionTypeNode(
                            [new TypeNode("i32"), new TypeNode("f64")],
                            new TypeNode("void")
                        ),
                        LiteralExpressionNode.Number(0)
                    )
                ]))
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function main(): void {
                var x: (i32, f64) => void = 0;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatAliasInterfaceTypeTest()
    {
        var tree = new SyntaxTree([
            new TypeAliasDeclarationNode(
                AccessModifier.Public,
                "Point",
                new InterfaceNode(
                    [
                        new InterfaceFieldNode("x", new TypeNode("i32")),
                        new InterfaceFieldNode("y", new TypeNode("i32")),
                    ],
                    [
                        new InterfaceMethodNode(
                            "distance",
                            [new ParameterNode("other", new TypeNode("Point"))],
                            new TypeNode("f64")
                        )
                    ]
                )
            )
        ]);
        var formatted = tree.ToString();
        const string expected = "public type Point = {\n    x: i32;\n    y: i32;\n    \n    distance(other: Point): f64;\n}";

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatInlineInterfaceTypeTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new VariableDeclarationStatementNode(
                        "p",
                        new InterfaceNode(
                            [
                                new InterfaceFieldNode("x", new TypeNode("i32")),
                                new InterfaceFieldNode("y", new TypeNode("i32")),
                            ],
                            []
                        ),
                        LiteralExpressionNode.Number(0)
                    )
                ])
            )
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function main(): void {
                var p: { x: i32; y: i32; } = 0;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatMultipleMemberAccessTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new MemberAccessExpressionNode(
                            new MemberAccessExpressionNode(
                                new MemberAccessExpressionNode("a"),
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
            function main(): void {
                return a.b.c;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatThisExpressionTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new MemberAccessExpressionNode("this")
                    )
                ])
            )
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function main(): void {
                return this;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatThisExpressionWithMemberAccessTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new MemberAccessExpressionNode(
                            new MemberAccessExpressionNode(
                                new MemberAccessExpressionNode("this"),
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
            function main(): void {
                return this.a.b;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatNewOperatorTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new VariableDeclarationStatementNode(
                        "p",
                        new TypeNode("Point"),
                        new NewExpressionNode(
                            new TypeNode("Point"),
                            [LiteralExpressionNode.Number(1), LiteralExpressionNode.Number(2)]
                        )
                    )
                ])
            )
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function main(): void {
                var p: Point = new Point(1, 2);
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatDiscriminatedUnionOfTypesTest()
    {
        var tree = new SyntaxTree([
            new TypeAliasDeclarationNode(
                AccessModifier.Public,
                "Numbers",
                new DiscriminatedUnionNode([
                    new TypeNode("i8"),
                    new TypeNode("i16"),
                    new TypeNode("i32"),
                    new TypeNode("i64"),
                ]))
        ]);
        var formatted = tree.ToString();
        const string expected = "public type Numbers = i8 | i16 | i32 | i64;";

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatDiscriminatedUnionOfFunctionsTest()
    {
        var tree = new SyntaxTree([
            new TypeAliasDeclarationNode(
                AccessModifier.Public,
                "F",
                new DiscriminatedUnionNode([
                    new FunctionTypeNode([], new TypeNode("void")),
                    new FunctionTypeNode([new TypeNode("i32"), new TypeNode("i32")], new TypeNode("i32"))
                ]))
        ]);
        var formatted = tree.ToString();
        const string expected = "public type F = () => void | (i32, i32) => i32;";

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatDiscriminatedUnionOfInterfacesTest()
    {
        var tree = new SyntaxTree([
            new TypeAliasDeclarationNode(
                AccessModifier.Public,
                "I",
                new DiscriminatedUnionNode([
                    new InterfaceNode([], []),
                    new InterfaceNode(
                        [
                            new InterfaceFieldNode("x", new TypeNode("i32")),
                            new InterfaceFieldNode("y", new TypeNode("i32")),
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
        var tree = new SyntaxTree([
            new TypeAliasDeclarationNode(
                AccessModifier.Public,
                "T",
                new DiscriminatedUnionNode([
                    new InterfaceNode([], []),
                    new TypeNode("i32"),
                    new FunctionTypeNode([], new TypeNode("void")),
                ]))
        ]);
        var formatted = tree.ToString();
        const string expected = "public type T = { } | i32 | () => void;";

        Assert.That(formatted, Is.EqualTo(expected));
    }

    [Test]
    public void FormatNullTest()
    {
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create("main", [], new TypeNode("void"),
                new BlockStatementNode([
                    new VariableDeclarationStatementNode(
                        "x",
                        new DiscriminatedUnionNode([new TypeNode("i32"), new TypeNode("null")]),
                        new NullExpressionNode()
                    )
                ]))
        ]);
        var formatted = tree.ToString();
        const string expected =
            """
            function main(): void {
                var x: i32 | null = null;
            }
            """;

        Assert.That(formatted, Is.EqualTo(expected));
    }
}