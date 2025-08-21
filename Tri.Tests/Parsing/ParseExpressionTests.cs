using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseExpressionTests
{
    [Test]
    public void ParseVariableTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                var x: i32 = 5;
            }
            """);

        var variableDeclarationNode = new VariableDeclarationStatementNode(
            "x",
            new TypeNode("i32"),
            new LiteralExpressionNode(LiteralExpressionKind.Number, 5)
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], new TypeNode("void"), new BlockStatementNode([variableDeclarationNode]))
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseUnaryPlusTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                var x: i32 = +2;
            }
            """);

        var variableDeclarationNode = new VariableDeclarationStatementNode(
            "x",
            new TypeNode("i32"),
            new UnaryExpressionNode(
                UnaryExpressionKind.UnaryPlus,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], new TypeNode("void"), new BlockStatementNode([variableDeclarationNode])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseUnaryMinusTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                var x: i32 = -2;
            }
            """);

        var variableDeclarationNode = new VariableDeclarationStatementNode(
            "x",
            new TypeNode("i32"),
            new UnaryExpressionNode(
                UnaryExpressionKind.UnaryMinus,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], new TypeNode("void"), new BlockStatementNode([variableDeclarationNode])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseLogicalNotTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                var x: i32 = !2;
            }
            """);

        var variableDeclarationNode = new VariableDeclarationStatementNode(
            "x",
            new TypeNode("i32"),
            new UnaryExpressionNode(
                UnaryExpressionKind.LogicalNot,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], new TypeNode("void"), new BlockStatementNode([variableDeclarationNode])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseBitwiseNotTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                var x: i32 = ~2;
            }
            """);

        var variableDeclarationNode = new VariableDeclarationStatementNode(
            "x",
            new TypeNode("i32"),
            new UnaryExpressionNode(
                UnaryExpressionKind.BitwiseNot,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], new TypeNode("void"), new BlockStatementNode([variableDeclarationNode])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void MultipleUnaryOperatorsTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                var x: i32 = ~+2;
            }
            """);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], new TypeNode("void"), new BlockStatementNode([
                    new VariableDeclarationStatementNode(
                        "x",
                        new TypeNode("i32"),
                        new UnaryExpressionNode(
                            UnaryExpressionKind.BitwiseNot,
                            new UnaryExpressionNode(
                                UnaryExpressionKind.UnaryPlus,
                                new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
                            )
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    [TestCase("+", BinaryExpressionKind.Addition)]
    [TestCase("-", BinaryExpressionKind.Subtraction)]
    [TestCase("*", BinaryExpressionKind.Multiplication)]
    [TestCase("/", BinaryExpressionKind.Division)]
    [TestCase("%", BinaryExpressionKind.Modulus)]
    public void ParseBinaryNumberTest(string @operator, BinaryExpressionKind kind)
    {
        var parse = new Parser();
        var tree = parse.Parse(
            $$"""
              function main(): void {
                  var x: i32 = 2 {{@operator}} 2;
              }
              """);

        var variableDeclarationNode = new VariableDeclarationStatementNode(
            "x",
            new TypeNode("i32"),
            new BinaryExpressionNode(
                kind,
                LiteralExpressionNode.Number(2),
                LiteralExpressionNode.Number(2)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], new TypeNode("void"), new BlockStatementNode([variableDeclarationNode])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseBitwiseAndTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                var x: i32 = 2 & 2;
            }
            """);

        var variableDeclarationNode = new VariableDeclarationStatementNode(
            "x",
            new TypeNode("i32"),
            new BinaryExpressionNode(
                BinaryExpressionKind.BitwiseAnd,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2),
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], new TypeNode("void"), new BlockStatementNode([variableDeclarationNode])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseBitwiseOrTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                var x: i32 = 2 | 2;
            }
            """);

        var variableDeclarationNode = new VariableDeclarationStatementNode(
            "x",
            new TypeNode("i32"),
            new BinaryExpressionNode(
                BinaryExpressionKind.BitwiseOr,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2),
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], new TypeNode("void"), new BlockStatementNode([variableDeclarationNode])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseBitwiseXorTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                var x: i32 = 2 ^ 2;
            }
            """);

        var variableDeclarationNode = new VariableDeclarationStatementNode(
            "x",
            new TypeNode("i32"),
            new BinaryExpressionNode(
                BinaryExpressionKind.BitwiseXor,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2),
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], new TypeNode("void"), new BlockStatementNode([variableDeclarationNode])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseLogicalAndTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                var x: bool = true && true;
            }
            """);

        var variableDeclarationNode = new VariableDeclarationStatementNode(
            "x",
            new TypeNode("bool"),
            new BinaryExpressionNode(
                BinaryExpressionKind.ConditionalAnd,
                new LiteralExpressionNode(LiteralExpressionKind.Boolean, true),
                new LiteralExpressionNode(LiteralExpressionKind.Boolean, true)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], new TypeNode("void"), new BlockStatementNode([
                    variableDeclarationNode
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseLogicalOrTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                var x: bool = true || true;
            }
            """);

        var variableDeclarationNode = new VariableDeclarationStatementNode(
            "x",
            new TypeNode("bool"),
            new BinaryExpressionNode(
                BinaryExpressionKind.ConditionalOr,
                new LiteralExpressionNode(LiteralExpressionKind.Boolean, true),
                new LiteralExpressionNode(LiteralExpressionKind.Boolean, true)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], new TypeNode("void"), new BlockStatementNode([variableDeclarationNode])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseEqualityTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                var x: bool = true == true;
            }
            """);

        var variableDeclarationNode = new VariableDeclarationStatementNode(
            "x",
            new TypeNode("bool"),
            new BinaryExpressionNode(
                BinaryExpressionKind.Equality,
                new LiteralExpressionNode(LiteralExpressionKind.Boolean, true),
                new LiteralExpressionNode(LiteralExpressionKind.Boolean, true)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], new TypeNode("void"), new BlockStatementNode([variableDeclarationNode])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseInequalityTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                var x: bool = true != true;
            }
            """);

        var variableDeclarationNode = new VariableDeclarationStatementNode(
            "x",
            new TypeNode("bool"),
            new BinaryExpressionNode(
                BinaryExpressionKind.Inequality,
                new LiteralExpressionNode(LiteralExpressionKind.Boolean, true),
                new LiteralExpressionNode(LiteralExpressionKind.Boolean, true)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], new TypeNode("void"), new BlockStatementNode([variableDeclarationNode])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseLessThanTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                var x: bool = 2 < 2;
            }
            """);

        var variableDeclarationNode = new VariableDeclarationStatementNode(
            "x",
            new TypeNode("bool"),
            new BinaryExpressionNode(
                BinaryExpressionKind.LessThan,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2),
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], new TypeNode("void"), new BlockStatementNode([variableDeclarationNode])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseLessThanOrEqualTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                var x: bool = 2 <= 2;
            }
            """);

        var variableDeclarationNode = new VariableDeclarationStatementNode(
            "x",
            new TypeNode("bool"),
            new BinaryExpressionNode(
                BinaryExpressionKind.LessThanOrEqual,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2),
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], new TypeNode("void"), new BlockStatementNode([
                    variableDeclarationNode
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseGreaterThanTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                var x: bool = 2 > 2;
            }
            """);

        var variableDeclarationNode = new VariableDeclarationStatementNode(
            "x",
            new TypeNode("bool"),
            new BinaryExpressionNode(
                BinaryExpressionKind.GreaterThan,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2),
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], new TypeNode("void"), new BlockStatementNode([variableDeclarationNode])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseGreaterThanOrEqualTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                var x: bool = 2 >= 2;
            }
            """);

        var variableDeclarationNode = new VariableDeclarationStatementNode(
            "x",
            new TypeNode("bool"),
            new BinaryExpressionNode(
                BinaryExpressionKind.GreaterThanOrEqual,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2),
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], new TypeNode("void"), new BlockStatementNode([variableDeclarationNode])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    [TestCase("=", BinaryExpressionKind.Assignment)]
    [TestCase("+=", BinaryExpressionKind.AdditionAssignment)]
    [TestCase("-=", BinaryExpressionKind.SubtractionAssignment)]
    [TestCase("*=", BinaryExpressionKind.MultiplicationAssignment)]
    [TestCase("/=", BinaryExpressionKind.DivisionAssignment)]
    [TestCase("%=", BinaryExpressionKind.ModulusAssignment)]
    [TestCase("&=", BinaryExpressionKind.BitwiseAndAssignment)]
    [TestCase("|=", BinaryExpressionKind.BitwiseOrAssignment)]
    [TestCase("^=", BinaryExpressionKind.BitwiseXorAssignment)]
    public void ParseAssignmentTest(string @operator, BinaryExpressionKind kind)
    {
        var parse = new Parser();
        var tree = parse.Parse(
            $$"""
              function main(): void {
                  x {{@operator}} 1;
              }
              """);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], new TypeNode("void"), new BlockStatementNode([
                    new ExpressionStatementNode(
                        new BinaryExpressionNode(
                            kind,
                            new MemberAccessExpressionNode("x"),
                            LiteralExpressionNode.Number(1)
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseParenTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                var x: i32 = (1 + 2) * 3;
            }
            """);

        var variableDeclarationNode = new VariableDeclarationStatementNode(
            "x",
            new TypeNode("i32"),
            new BinaryExpressionNode(
                BinaryExpressionKind.Multiplication,
                new BinaryExpressionNode(
                    BinaryExpressionKind.Addition,
                    new LiteralExpressionNode(LiteralExpressionKind.Number, 1),
                    new LiteralExpressionNode(LiteralExpressionKind.Number, 2)),
                new LiteralExpressionNode(LiteralExpressionKind.Number, 3)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], new TypeNode("void"), new BlockStatementNode([variableDeclarationNode])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseVariableExpressionTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(y: i32): void {
                var x: i32 = 2 * y;
            }
            """);

        var variableDeclarationNode = new VariableDeclarationStatementNode(
            "x",
            new TypeNode("i32"),
            new BinaryExpressionNode(
                BinaryExpressionKind.Multiplication,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2),
                new MemberAccessExpressionNode("y")
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [new ParameterNode("y", new TypeNode("i32"))],
                new TypeNode("void"),
                new BlockStatementNode([variableDeclarationNode])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParsePrecedenceTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                var x: i32 = true || true && false | false ^ false & true == 1 + 2 * 3 < 10;
            }
            """);

        var variableDeclarationNode = new VariableDeclarationStatementNode(
            "x",
            new TypeNode("i32"),
            new BinaryExpressionNode(
                BinaryExpressionKind.ConditionalOr,
                new LiteralExpressionNode(LiteralExpressionKind.Boolean, true),
                new BinaryExpressionNode(
                    BinaryExpressionKind.ConditionalAnd,
                    new LiteralExpressionNode(LiteralExpressionKind.Boolean, true),
                    new BinaryExpressionNode(
                        BinaryExpressionKind.BitwiseOr,
                        new LiteralExpressionNode(LiteralExpressionKind.Boolean, false),
                        new BinaryExpressionNode(
                            BinaryExpressionKind.BitwiseXor,
                            new LiteralExpressionNode(LiteralExpressionKind.Boolean, false),
                            new BinaryExpressionNode(
                                BinaryExpressionKind.BitwiseAnd,
                                new LiteralExpressionNode(LiteralExpressionKind.Boolean, false),
                                new BinaryExpressionNode(
                                    BinaryExpressionKind.Equality,
                                    new LiteralExpressionNode(LiteralExpressionKind.Boolean, true),
                                    new BinaryExpressionNode(
                                        BinaryExpressionKind.LessThan,
                                        new BinaryExpressionNode(
                                            BinaryExpressionKind.Addition,
                                            new LiteralExpressionNode(LiteralExpressionKind.Number, 1),
                                            new BinaryExpressionNode(
                                                BinaryExpressionKind.Multiplication,
                                                new LiteralExpressionNode(LiteralExpressionKind.Number, 2),
                                                new LiteralExpressionNode(LiteralExpressionKind.Number, 3)
                                            )
                                        ),
                                        new LiteralExpressionNode(LiteralExpressionKind.Number, 10)
                                    )
                                )
                            )
                        )
                    )
                )
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], new TypeNode("void"), new BlockStatementNode([variableDeclarationNode])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void TupleExpressionTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                return (1, 2);
            }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new TupleExpressionNode([
                            LiteralExpressionNode.Number(1),
                            LiteralExpressionNode.Number(2),
                        ])
                    )
                ]))
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void TupleExpressionMissingCloseParenTest()
    {
        var parse = new Parser();
        const string code =
            """
            function main(): void {
                return (1, 2;
            }
            """;

        Assert.That(
            () => parse.Parse(code),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected a close parenthesis."));
    }

    [Test]
    public void ParseNewArrayTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                return new i32[10];
            }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new NewArrayExpressionNode(
                            new ArrayTypeNode(new TypeNode("i32")),
                            LiteralExpressionNode.Number(10)
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseAsExpressionTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            function test(a: i32): i8 {
                return a is i8;
            }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "test",
                [new ParameterNode("a", new TypeNode("i32"))],
                new TypeNode("i8"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new IsExpressionNode(
                            new MemberAccessExpressionNode("a"),
                            new TypeNode("i8")
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseAsExpressionMissingTypeTest()
    {
        var parser = new Parser();
        const string code =
            """
            function test(a: i32): i8 {
                return a is;
            }
            """;

        Assert.That(
            () => parser.Parse(code),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected a type."));
    }

    [Test]
    public void ParseCastExpressionTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            function test(a: i32): i8 {
                return (i8)a;
            }
            """);
        var expected = new SyntaxTree([
            new FunctionDeclarationNode(
                "test",
                [new ParameterNode("a", new TypeNode("i32"))],
                new TypeNode("i8"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new CastExpressionNode(
                            new TypeNode("i8"),
                            new MemberAccessExpressionNode("a")
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseCastExpressionMissingCloseParenTest()
    {
        var parser = new Parser();
        const string code =
            """
            function test(a: i32): i8 {
                return (i8 a;
            }
            """;

        Assert.That(
            () => parser.Parse(code),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected a close parenthesis."));
    }

    [Test]
    public void ParseCastExpressionMissingExpressionTest()
    {
        var parser = new Parser();
        const string code =
            """
            function test(a: i32): i8 {
                return (i8);
            }
            """;

        Assert.That(
            () => parser.Parse(code),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected an expression."));
    }
}