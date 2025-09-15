using Trilang;
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

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(45, 3, 2)),
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(45, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(43, 2, 20)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(35, 2, 12), new SourcePosition(38, 2, 15)), "i32"),
                        LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(41, 2, 18), new SourcePosition(42, 2, 19)), 5)
                    )
                ])
            )
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

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(46, 3, 2)),
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(46, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(44, 2, 21)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(35, 2, 12), new SourcePosition(38, 2, 15)), "i32"),
                        new UnaryExpressionNode(
                            new SourceSpan(new SourcePosition(41, 2, 18), new SourcePosition(43, 2, 20)),
                            UnaryExpressionKind.UnaryPlus,
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(42, 2, 19), new SourcePosition(43, 2, 20)), 2)
                        )
                    )
                ])
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

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(46, 3, 2)),
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(46, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(44, 2, 21)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(35, 2, 12), new SourcePosition(38, 2, 15)), "i32"),
                        new UnaryExpressionNode(
                            new SourceSpan(new SourcePosition(41, 2, 18), new SourcePosition(43, 2, 20)),
                            UnaryExpressionKind.UnaryMinus,
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(42, 2, 19), new SourcePosition(43, 2, 20)), 2)
                        )
                    )
                ])
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

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(46, 3, 2)),
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(46, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(44, 2, 21)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(35, 2, 12), new SourcePosition(38, 2, 15)), "i32"),
                        new UnaryExpressionNode(
                            new SourceSpan(new SourcePosition(41, 2, 18), new SourcePosition(43, 2, 20)),
                            UnaryExpressionKind.LogicalNot,
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(42, 2, 19), new SourcePosition(43, 2, 20)), 2)
                        )
                    )
                ])
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

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(46, 3, 2)),
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(46, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(44, 2, 21)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(35, 2, 12), new SourcePosition(38, 2, 15)), "i32"),
                        new UnaryExpressionNode(
                            new SourceSpan(new SourcePosition(41, 2, 18), new SourcePosition(43, 2, 20)),
                            UnaryExpressionKind.BitwiseNot,
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(42, 2, 19), new SourcePosition(43, 2, 20)), 2)
                        )
                    )
                ])
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
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(47, 3, 2)),
                "main",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)),
                    "void"
                ),
                new BlockStatementNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(47, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(45, 2, 22)),
                        "x",
                        new TypeNode(
                            new SourceSpan(new SourcePosition(35, 2, 12), new SourcePosition(38, 2, 15)),
                            "i32"
                        ),
                        new UnaryExpressionNode(
                            new SourceSpan(new SourcePosition(41, 2, 18), new SourcePosition(44, 2, 21)),
                            UnaryExpressionKind.BitwiseNot,
                            new UnaryExpressionNode(
                                new SourceSpan(new SourcePosition(42, 2, 19), new SourcePosition(44, 2, 21)),
                                UnaryExpressionKind.UnaryPlus,
                                LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(43, 2, 20), new SourcePosition(44, 2, 21)), 2)
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

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(49, 3, 2)),
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(49, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(47, 2, 24)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(35, 2, 12), new SourcePosition(38, 2, 15)), "i32"),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(41, 2, 18), new SourcePosition(46, 2, 23)),
                            kind,
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(41, 2, 18), new SourcePosition(42, 2, 19)), 2),
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(45, 2, 22), new SourcePosition(46, 2, 23)), 2)
                        )
                    )
                ])
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

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(49, 3, 2)),
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)), "void"),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(49, 3, 2)),
                    [
                        new VariableDeclarationNode(
                            new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(47, 2, 24)),
                            "x",
                            new TypeNode(new SourceSpan(new SourcePosition(35, 2, 12), new SourcePosition(38, 2, 15)), "i32"),
                            new BinaryExpressionNode(
                                new SourceSpan(new SourcePosition(41, 2, 18), new SourcePosition(46, 2, 23)),
                                BinaryExpressionKind.BitwiseAnd,
                                LiteralExpressionNode.Integer(
                                    new SourceSpan(new SourcePosition(41, 2, 18), new SourcePosition(42, 2, 19)),
                                    2
                                ),
                                LiteralExpressionNode.Integer(
                                    new SourceSpan(new SourcePosition(45, 2, 22), new SourcePosition(46, 2, 23)),
                                    2
                                )
                            )
                        )
                    ])
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

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(49, 3, 2)),
                "main",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(49, 3, 2)),
                    [
                        new VariableDeclarationNode(
                            new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(47, 2, 24)),
                            "x",
                            new TypeNode(
                                new SourceSpan(new SourcePosition(35, 2, 12), new SourcePosition(38, 2, 15)),
                                "i32"
                            ),
                            new BinaryExpressionNode(
                                new SourceSpan(new SourcePosition(41, 2, 18), new SourcePosition(46, 2, 23)),
                                BinaryExpressionKind.BitwiseOr,
                                LiteralExpressionNode.Integer(
                                    new SourceSpan(new SourcePosition(41, 2, 18), new SourcePosition(42, 2, 19)),
                                    2
                                ),
                                LiteralExpressionNode.Integer(
                                    new SourceSpan(new SourcePosition(45, 2, 22), new SourcePosition(46, 2, 23)),
                                    2
                                )
                            )
                        )
                    ]
                )
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

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(49, 3, 2)),
                "main",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(49, 3, 2)),
                    [
                        new VariableDeclarationNode(
                            new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(47, 2, 24)),
                            "x",
                            new TypeNode(
                                new SourceSpan(new SourcePosition(35, 2, 12), new SourcePosition(38, 2, 15)),
                                "i32"
                            ),
                            new BinaryExpressionNode(
                                new SourceSpan(new SourcePosition(41, 2, 18), new SourcePosition(46, 2, 23)),
                                BinaryExpressionKind.BitwiseXor,
                                LiteralExpressionNode.Integer(
                                    new SourceSpan(new SourcePosition(41, 2, 18), new SourcePosition(42, 2, 19)),
                                    2
                                ),
                                LiteralExpressionNode.Integer(
                                    new SourceSpan(new SourcePosition(45, 2, 22), new SourcePosition(46, 2, 23)),
                                    2
                                )
                            )
                        )
                    ]
                )
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

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(57, 3, 2)),
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(57, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(55, 2, 32)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(35, 2, 12), new SourcePosition(39, 2, 16)), "bool"),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(42, 2, 19), new SourcePosition(54, 2, 31)),
                            BinaryExpressionKind.ConditionalAnd,
                            LiteralExpressionNode.True(new SourceSpan(new SourcePosition(42, 2, 19), new SourcePosition(46, 2, 23))),
                            LiteralExpressionNode.True(new SourceSpan(new SourcePosition(50, 2, 27), new SourcePosition(54, 2, 31)))
                        )
                    )
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

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(57, 3, 2)),
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(57, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(55, 2, 32)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(35, 2, 12), new SourcePosition(39, 2, 16)), "bool"),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(42, 2, 19), new SourcePosition(54, 2, 31)),
                            BinaryExpressionKind.ConditionalOr,
                            LiteralExpressionNode.True(new SourceSpan(new SourcePosition(42, 2, 19), new SourcePosition(46, 2, 23))),
                            LiteralExpressionNode.True(new SourceSpan(new SourcePosition(50, 2, 27), new SourcePosition(54, 2, 31)))
                        )
                    )
                ])
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

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(57, 3, 2)),
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(57, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(55, 2, 32)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(35, 2, 12), new SourcePosition(39, 2, 16)), "bool"),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(42, 2, 19), new SourcePosition(54, 2, 31)),
                            BinaryExpressionKind.Equality,
                            LiteralExpressionNode.True(new SourceSpan(new SourcePosition(42, 2, 19), new SourcePosition(46, 2, 23))),
                            LiteralExpressionNode.True(new SourceSpan(new SourcePosition(50, 2, 27), new SourcePosition(54, 2, 31)))
                        )
                    )
                ])
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

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(57, 3, 2)),
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(57, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(55, 2, 32)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(35, 2, 12), new SourcePosition(39, 2, 16)), "bool"),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(42, 2, 19), new SourcePosition(54, 2, 31)),
                            BinaryExpressionKind.Inequality,
                            LiteralExpressionNode.True(new SourceSpan(new SourcePosition(42, 2, 19), new SourcePosition(46, 2, 23))),
                            LiteralExpressionNode.True(new SourceSpan(new SourcePosition(50, 2, 27), new SourcePosition(54, 2, 31)))
                        )
                    )
                ])
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

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(50, 3, 2)),
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(50, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(48, 2, 25)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(35, 2, 12), new SourcePosition(39, 2, 16)), "bool"),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(42, 2, 19), new SourcePosition(47, 2, 24)),
                            BinaryExpressionKind.LessThan,
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(42, 2, 19), new SourcePosition(43, 2, 20)), 2),
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(46, 2, 23), new SourcePosition(47, 2, 24)), 2)
                        )
                    )
                ])
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

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(51, 3, 2)),
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(51, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(49, 2, 26)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(35, 2, 12), new SourcePosition(39, 2, 16)), "bool"),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(42, 2, 19), new SourcePosition(48, 2, 25)),
                            BinaryExpressionKind.LessThanOrEqual,
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(42, 2, 19), new SourcePosition(43, 2, 20)), 2),
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(47, 2, 24), new SourcePosition(48, 2, 25)), 2)
                        )
                    )
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

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(50, 3, 2)),
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(50, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(48, 2, 25)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(35, 2, 12), new SourcePosition(39, 2, 16)), "bool"),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(42, 2, 19), new SourcePosition(47, 2, 24)),
                            BinaryExpressionKind.GreaterThan,
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(42, 2, 19), new SourcePosition(43, 2, 20)), 2),
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(46, 2, 23), new SourcePosition(47, 2, 24)), 2)
                        )
                    )
                ])
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

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(51, 3, 2)),
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(51, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(49, 2, 26)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(35, 2, 12), new SourcePosition(39, 2, 16)), "bool"),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(42, 2, 19), new SourcePosition(48, 2, 25)),
                            BinaryExpressionKind.GreaterThanOrEqual,
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(42, 2, 19), new SourcePosition(43, 2, 20)), 2),
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(47, 2, 24), new SourcePosition(48, 2, 25)), 2)
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseAssignmentTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                x = 1;
            }
            """);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(36, 3, 2)),
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(36, 3, 2)), [
                    new ExpressionStatementNode(
                        new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(34, 2, 11)),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(33, 2, 10)),
                            BinaryExpressionKind.Assignment,
                            new MemberAccessExpressionNode(new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(29, 2, 6)), "x"),
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(32, 2, 9), new SourcePosition(33, 2, 10)), 1)
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
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
        var parse = new Parser();
        var tree = parse.Parse(
            $$"""
              function main(): void {
                  x {{@operator}} 1;
              }
              """);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(37, 3, 2)),
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(37, 3, 2)), [
                    new ExpressionStatementNode(
                        new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(35, 2, 12)),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(34, 2, 11)),
                            kind,
                            new MemberAccessExpressionNode(new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(29, 2, 6)), "x"),
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(33, 2, 10), new SourcePosition(34, 2, 11)), 1)
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

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(55, 3, 2)),
                "main",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(55, 3, 2)),
                    [
                        new VariableDeclarationNode(
                            new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(53, 2, 30)),
                            "x",
                            new TypeNode(
                                new SourceSpan(new SourcePosition(35, 2, 12), new SourcePosition(38, 2, 15)),
                                "i32"
                            ),
                            new BinaryExpressionNode(
                                new SourceSpan(new SourcePosition(42, 2, 19), new SourcePosition(52, 2, 29)),
                                BinaryExpressionKind.Multiplication,
                                new BinaryExpressionNode(
                                    new SourceSpan(new SourcePosition(42, 2, 19), new SourcePosition(47, 2, 24)),
                                    BinaryExpressionKind.Addition,
                                    LiteralExpressionNode.Integer(
                                        new SourceSpan(new SourcePosition(42, 2, 19), new SourcePosition(43, 2, 20)),
                                        1
                                    ),
                                    LiteralExpressionNode.Integer(
                                        new SourceSpan(new SourcePosition(46, 2, 23), new SourcePosition(47, 2, 24)),
                                        2
                                    )
                                ),
                                LiteralExpressionNode.Integer(
                                    new SourceSpan(new SourcePosition(51, 2, 28), new SourcePosition(52, 2, 29)),
                                    3
                                )
                            )
                        )
                    ])
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

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(55, 3, 2)),
                "main",
                [new ParameterNode(new SourceSpan(new SourcePosition(14, 1, 15), new SourcePosition(20, 1, 21)), "y", new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(20, 1, 21)), "i32"))],
                new TypeNode(new SourceSpan(new SourcePosition(23, 1, 24), new SourcePosition(27, 1, 28)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(28, 1, 29), new SourcePosition(55, 3, 2)), [
                    new VariableDeclarationNode(
                        new SourceSpan(new SourcePosition(34, 2, 5), new SourcePosition(53, 2, 24)),
                        "x",
                        new TypeNode(new SourceSpan(new SourcePosition(41, 2, 12), new SourcePosition(44, 2, 15)), "i32"),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(47, 2, 18), new SourcePosition(52, 2, 23)),
                            BinaryExpressionKind.Multiplication,
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(47, 2, 18), new SourcePosition(48, 2, 19)), 2),
                            new MemberAccessExpressionNode(new SourceSpan(new SourcePosition(51, 2, 22), new SourcePosition(52, 2, 23)), "y")
                        )
                    )
                ])
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

        var variableDeclarationNode = new VariableDeclarationNode(
            new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(104, 2, 81)),
            "x",
            new TypeNode(new SourceSpan(new SourcePosition(35, 2, 12), new SourcePosition(38, 2, 15)), "i32"),
            new BinaryExpressionNode(
                new SourceSpan(new SourcePosition(41, 2, 18), new SourcePosition(103, 2, 80)),
                BinaryExpressionKind.ConditionalOr,
                LiteralExpressionNode.True(new SourceSpan(new SourcePosition(41, 2, 18), new SourcePosition(45, 2, 22))),
                new BinaryExpressionNode(
                    new SourceSpan(new SourcePosition(49, 2, 26), new SourcePosition(103, 2, 80)),
                    BinaryExpressionKind.ConditionalAnd,
                    LiteralExpressionNode.True(new SourceSpan(new SourcePosition(49, 2, 26), new SourcePosition(53, 2, 30))),
                    new BinaryExpressionNode(
                        new SourceSpan(new SourcePosition(57, 2, 34), new SourcePosition(103, 2, 80)),
                        BinaryExpressionKind.BitwiseOr,
                        LiteralExpressionNode.False(new SourceSpan(new SourcePosition(57, 2, 34), new SourcePosition(62, 2, 39))),
                        new BinaryExpressionNode(
                            new SourceSpan(new SourcePosition(65, 2, 42), new SourcePosition(103, 2, 80)),
                            BinaryExpressionKind.BitwiseXor,
                            LiteralExpressionNode.False(new SourceSpan(new SourcePosition(65, 2, 42), new SourcePosition(70, 2, 47))),
                            new BinaryExpressionNode(
                                new SourceSpan(new SourcePosition(73, 2, 50), new SourcePosition(103, 2, 80)),
                                BinaryExpressionKind.BitwiseAnd,
                                LiteralExpressionNode.False(new SourceSpan(new SourcePosition(73, 2, 50), new SourcePosition(78, 2, 55))),
                                new BinaryExpressionNode(
                                    new SourceSpan(new SourcePosition(81, 2, 58), new SourcePosition(103, 2, 80)),
                                    BinaryExpressionKind.Equality,
                                    LiteralExpressionNode.True(new SourceSpan(new SourcePosition(81, 2, 58), new SourcePosition(85, 2, 62))),
                                    new BinaryExpressionNode(
                                        new SourceSpan(new SourcePosition(89, 2, 66), new SourcePosition(103, 2, 80)),
                                        BinaryExpressionKind.LessThan,
                                        new BinaryExpressionNode(
                                            new SourceSpan(new SourcePosition(89, 2, 66), new SourcePosition(98, 2, 75)),
                                            BinaryExpressionKind.Addition,
                                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(89, 2, 66), new SourcePosition(90, 2, 67)), 1),
                                            new BinaryExpressionNode(
                                                new SourceSpan(new SourcePosition(93, 2, 70), new SourcePosition(98, 2, 75)),
                                                BinaryExpressionKind.Multiplication,
                                                LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(93, 2, 70), new SourcePosition(94, 2, 71)), 2),
                                                LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(97, 2, 74), new SourcePosition(98, 2, 75)), 3)
                                            )
                                        ),
                                        LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(101, 2, 78), new SourcePosition(103, 2, 80)), 10)
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
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(106, 3, 2)),
                "main", [], new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)), "void"), new BlockStatementNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(106, 3, 2)), [variableDeclarationNode])
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
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(44, 3, 2)),
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(44, 3, 2)), [
                    new ReturnStatementNode(
                        new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(42, 2, 19)),
                        new TupleExpressionNode(new SourceSpan(new SourcePosition(35, 2, 12), new SourcePosition(41, 2, 18)), [
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(36, 2, 13), new SourcePosition(37, 2, 14)), 1),
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(39, 2, 16), new SourcePosition(40, 2, 17)), 2),
                        ])
                    )
                ])
            )
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
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(49, 3, 2)),
                "main",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(49, 3, 2)), [
                    new ReturnStatementNode(
                        new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(47, 2, 24)),
                        new NewArrayExpressionNode(
                            new SourceSpan(new SourcePosition(35, 2, 12), new SourcePosition(46, 2, 23)),
                            new ArrayTypeNode(new SourceSpan(new SourcePosition(39, 2, 16), new SourcePosition(42, 2, 19)), new TypeNode(new SourceSpan(new SourcePosition(39, 2, 16), new SourcePosition(42, 2, 19)), "i32")),
                            LiteralExpressionNode.Integer(new SourceSpan(new SourcePosition(43, 2, 20), new SourcePosition(45, 2, 22)), 10)
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
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(49, 3, 2)),
                "test",
                [new ParameterNode(new SourceSpan(new SourcePosition(14, 1, 15), new SourcePosition(20, 1, 21)), "a", new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(20, 1, 21)), "i32"))],
                new TypeNode(new SourceSpan(new SourcePosition(23, 1, 24), new SourcePosition(25, 1, 26)), "i8"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(26, 1, 27), new SourcePosition(49, 3, 2)), [
                    new ReturnStatementNode(
                        new SourceSpan(new SourcePosition(32, 2, 5), new SourcePosition(47, 2, 20)),
                        new IsExpressionNode(
                            new MemberAccessExpressionNode(new SourceSpan(new SourcePosition(39, 2, 12), new SourcePosition(40, 2, 13)), "a"),
                            new TypeNode(new SourceSpan(new SourcePosition(44, 2, 17), new SourcePosition(46, 2, 19)), "i8")
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
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(47, 3, 2)),
                "test",
                [new ParameterNode(new SourceSpan(new SourcePosition(14, 1, 15), new SourcePosition(20, 1, 21)), "a", new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(20, 1, 21)), "i32"))],
                new TypeNode(new SourceSpan(new SourcePosition(23, 1, 24), new SourcePosition(25, 1, 26)), "i8"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(26, 1, 27), new SourcePosition(47, 3, 2)), [
                    new ReturnStatementNode(
                        new SourceSpan(new SourcePosition(32, 2, 5), new SourcePosition(45, 2, 18)),
                        new CastExpressionNode(
                            new SourceSpan(new SourcePosition(39, 2, 12), new SourcePosition(44, 2, 17)),
                            new TypeNode(new SourceSpan(new SourcePosition(40, 2, 13), new SourcePosition(42, 2, 15)), "i8"),
                            new MemberAccessExpressionNode(new SourceSpan(new SourcePosition(43, 2, 16), new SourcePosition(44, 2, 17)), "a")
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

    [Test]
    public void ParseFloatingNumberTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            function test(): f64 {
                return 3.14;
            }
            """);
        var expected = new SyntaxTree([
            new FunctionDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(41, 3, 2)),
                "test",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(20, 1, 21)), "f64"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(41, 3, 2)), [
                    new ReturnStatementNode(
                        new SourceSpan(new SourcePosition(27, 2, 5), new SourcePosition(39, 2, 17)),
                        LiteralExpressionNode.Float(new SourceSpan(new SourcePosition(34, 2, 12), new SourcePosition(38, 2, 16)), 3.14)
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }
}