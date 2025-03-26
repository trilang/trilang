using Trilang.Parsing.Nodes;
using Trilang.Symbols;

namespace Tri.Tests.Parsing;

public class CommonFormatterTests
{
    [Test]
    public void FormatEmptyFunctionTest()
    {
        var rootTable = new SymbolTable();
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create("main", [], "void", new BlockStatementNode(rootTable.CreateChild()))
        ], rootTable);
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
        var rootTable = new SymbolTable();
        var tree = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "add", [
                    new FunctionParameterNode("x", "i32"),
                    new FunctionParameterNode("y", "i32"),
                ],
                "void",
                new BlockStatementNode(rootTable.CreateChild())),
            FunctionDeclarationNode.Create("main", [], "void", new BlockStatementNode(rootTable.CreateChild())),
        ], rootTable);
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
        var rootTable = new SymbolTable();
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
                ], rootTable.CreateChild())
            ),
        ], rootTable);
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
        var rootTable = new SymbolTable();
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
                ], rootTable.CreateChild())
            ),
        ], rootTable);
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
        var rootTable = new SymbolTable();
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
                ], rootTable.CreateChild())
            ),
        ], rootTable);
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
        var rootTable = new SymbolTable();
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
                ], rootTable.CreateChild())
            ),
        ], rootTable);
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
        var rootTable = new SymbolTable();
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
                ], rootTable.CreateChild())
            ),
        ], rootTable);
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
        var rootTable = new SymbolTable();
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
                ], rootTable.CreateChild())
            ),
        ], rootTable);
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
        var rootTable = new SymbolTable();
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
                ], rootTable.CreateChild())
            ),
        ], rootTable);
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
        var rootTable = new SymbolTable();
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
                ], rootTable.CreateChild())
            ),
        ], rootTable);
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
        var rootTable = new SymbolTable();
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
                ], rootTable.CreateChild())
            ),
        ], rootTable);
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
        var rootTable = new SymbolTable();
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
                ], rootTable.CreateChild())
            ),
        ], rootTable);
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
        var rootTable = new SymbolTable();
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
                ], rootTable.CreateChild())
            ),
        ], rootTable);
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
        var rootTable = new SymbolTable();
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
                ], rootTable.CreateChild())
            ),
        ], rootTable);
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
        var rootTable = new SymbolTable();
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
                ], rootTable.CreateChild())
            ),
        ], rootTable);
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
        var rootTable = new SymbolTable();
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
                ], rootTable.CreateChild())
            ),
        ], rootTable);
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
        var rootTable = new SymbolTable();
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
                ], rootTable.CreateChild())
            ),
        ], rootTable);
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
        var rootTable = new SymbolTable();
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
                ], rootTable.CreateChild())
            ),
        ], rootTable);
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
        var rootTable = new SymbolTable();
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
                ], rootTable.CreateChild())
            ),
        ], rootTable);
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
        var rootTable = new SymbolTable();
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
                ], rootTable.CreateChild())
            ),
        ], rootTable);
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
    public void FormatIfTest()
    {
        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
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
                        ], funcTable.CreateChild())
                    ),
                    new ReturnStatementNode(
                        new VariableExpressionNode("x")
                    )
                ], funcTable)
            ),
        ], rootTable);
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
        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
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
                        ], funcTable.CreateChild()),
                        new BlockStatementNode([
                            new ReturnStatementNode(
                                new VariableExpressionNode("x")
                            )
                        ], funcTable.CreateChild())
                    )
                ], funcTable)
            ),
        ], rootTable);
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
        var rootTable = new SymbolTable();
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
                ], rootTable.CreateChild())),
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
                ], rootTable.CreateChild())),
        ], rootTable);
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
}