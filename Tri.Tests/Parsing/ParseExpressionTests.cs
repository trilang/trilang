using Trilang.Parsing;
using Trilang.Parsing.Nodes;
using Trilang.Symbols;

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

        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
        var variableDeclarationNode = new VariableDeclarationNode(
            "x",
            "i32",
            new LiteralExpressionNode(LiteralExpressionKind.Number, 5)
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], "void", new BlockStatementNode([variableDeclarationNode], funcTable))
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));
        funcTable.TryAddVariable(new VariableSymbol("x", variableDeclarationNode));

        Assert.That(tree, Is.EqualTo(expected));
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

        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
        var variableDeclarationNode = new VariableDeclarationNode(
            "x",
            "i32",
            new UnaryExpressionNode(
                UnaryExpressionKind.UnaryPlus,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], "void", new BlockStatementNode([variableDeclarationNode], funcTable)
            )
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));
        funcTable.TryAddVariable(new VariableSymbol("x", variableDeclarationNode));

        Assert.That(tree, Is.EqualTo(expected));
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

        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
        var variableDeclarationNode = new VariableDeclarationNode(
            "x",
            "i32",
            new UnaryExpressionNode(
                UnaryExpressionKind.UnaryMinus,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], "void", new BlockStatementNode([variableDeclarationNode], funcTable)
            )
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));
        funcTable.TryAddVariable(new VariableSymbol("x", variableDeclarationNode));

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseAdditionTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                var x: i32 = 2 + 2;
            }
            """);

        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
        var variableDeclarationNode = new VariableDeclarationNode(
            "x",
            "i32",
            new BinaryExpressionNode(
                BinaryExpressionKind.Addition,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2),
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], "void", new BlockStatementNode([variableDeclarationNode], funcTable)
            )
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));
        funcTable.TryAddVariable(new VariableSymbol("x", variableDeclarationNode));

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseSubtractionTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                var x: i32 = 2 - 2;
            }
            """);

        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
        var variableDeclarationNode = new VariableDeclarationNode(
            "x",
            "i32",
            new BinaryExpressionNode(
                BinaryExpressionKind.Subtraction,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2),
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], "void", new BlockStatementNode([variableDeclarationNode], funcTable)
            )
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));
        funcTable.TryAddVariable(new VariableSymbol("x", variableDeclarationNode));

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseMultiplicationTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                var x: i32 = 2 * 2;
            }
            """);

        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
        var variableDeclarationNode = new VariableDeclarationNode(
            "x",
            "i32",
            new BinaryExpressionNode(
                BinaryExpressionKind.Multiplication,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2),
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], "void", new BlockStatementNode([variableDeclarationNode], funcTable)
            )
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));
        funcTable.TryAddVariable(new VariableSymbol("x", variableDeclarationNode));

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseDivisionTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                var x: i32 = 2 / 2;
            }
            """);

        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
        var variableDeclarationNode = new VariableDeclarationNode(
            "x",
            "i32",
            new BinaryExpressionNode(
                BinaryExpressionKind.Division,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2),
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], "void", new BlockStatementNode([variableDeclarationNode], funcTable)
            )
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));
        funcTable.TryAddVariable(new VariableSymbol("x", variableDeclarationNode));

        Assert.That(tree, Is.EqualTo(expected));
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

        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
        var variableDeclarationNode = new VariableDeclarationNode(
            "x",
            "i32",
            new BinaryExpressionNode(
                BinaryExpressionKind.BitwiseAnd,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2),
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], "void", new BlockStatementNode([variableDeclarationNode], funcTable)
            )
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));
        funcTable.TryAddVariable(new VariableSymbol("x", variableDeclarationNode));

        Assert.That(tree, Is.EqualTo(expected));
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

        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
        var variableDeclarationNode = new VariableDeclarationNode(
            "x",
            "i32",
            new BinaryExpressionNode(
                BinaryExpressionKind.BitwiseOr,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2),
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], "void", new BlockStatementNode([variableDeclarationNode], funcTable)
            )
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));
        funcTable.TryAddVariable(new VariableSymbol("x", variableDeclarationNode));

        Assert.That(tree, Is.EqualTo(expected));
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

        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
        var variableDeclarationNode = new VariableDeclarationNode(
            "x",
            "i32",
            new BinaryExpressionNode(
                BinaryExpressionKind.BitwiseXor,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2),
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], "void", new BlockStatementNode([variableDeclarationNode], funcTable)
            )
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));
        funcTable.TryAddVariable(new VariableSymbol("x", variableDeclarationNode));

        Assert.That(tree, Is.EqualTo(expected));
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
        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
        var variableDeclarationNode = new VariableDeclarationNode(
            "x",
            "bool",
            new BinaryExpressionNode(
                BinaryExpressionKind.ConditionalAnd,
                new LiteralExpressionNode(LiteralExpressionKind.Boolean, true),
                new LiteralExpressionNode(LiteralExpressionKind.Boolean, true)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], "void", new BlockStatementNode([
                    variableDeclarationNode
                ], funcTable)
            )
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));
        funcTable.TryAddVariable(new VariableSymbol("x", variableDeclarationNode));

        Assert.That(tree, Is.EqualTo(expected));
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

        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
        var variableDeclarationNode = new VariableDeclarationNode(
            "x",
            "bool",
            new BinaryExpressionNode(
                BinaryExpressionKind.ConditionalOr,
                new LiteralExpressionNode(LiteralExpressionKind.Boolean, true),
                new LiteralExpressionNode(LiteralExpressionKind.Boolean, true)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], "void", new BlockStatementNode([variableDeclarationNode], funcTable)
            )
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));
        funcTable.TryAddVariable(new VariableSymbol("x", variableDeclarationNode));

        Assert.That(tree, Is.EqualTo(expected));
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

        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
        var variableDeclarationNode = new VariableDeclarationNode(
            "x",
            "bool",
            new BinaryExpressionNode(
                BinaryExpressionKind.Equality,
                new LiteralExpressionNode(LiteralExpressionKind.Boolean, true),
                new LiteralExpressionNode(LiteralExpressionKind.Boolean, true)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], "void", new BlockStatementNode([variableDeclarationNode], funcTable)
            )
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));
        funcTable.TryAddVariable(new VariableSymbol("x", variableDeclarationNode));

        Assert.That(tree, Is.EqualTo(expected));
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

        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
        var variableDeclarationNode = new VariableDeclarationNode(
            "x",
            "bool",
            new BinaryExpressionNode(
                BinaryExpressionKind.Inequality,
                new LiteralExpressionNode(LiteralExpressionKind.Boolean, true),
                new LiteralExpressionNode(LiteralExpressionKind.Boolean, true)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], "void", new BlockStatementNode([variableDeclarationNode], funcTable)
            )
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));
        funcTable.TryAddVariable(new VariableSymbol("x", variableDeclarationNode));

        Assert.That(tree, Is.EqualTo(expected));
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

        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
        var variableDeclarationNode = new VariableDeclarationNode(
            "x",
            "bool",
            new BinaryExpressionNode(
                BinaryExpressionKind.LessThan,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2),
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], "void", new BlockStatementNode([variableDeclarationNode], funcTable)
            )
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));
        funcTable.TryAddVariable(new VariableSymbol("x", variableDeclarationNode));

        Assert.That(tree, Is.EqualTo(expected));
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

        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
        var variableDeclarationNode = new VariableDeclarationNode(
            "x",
            "bool",
            new BinaryExpressionNode(
                BinaryExpressionKind.LessThanOrEqual,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2),
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], "void", new BlockStatementNode([
                    variableDeclarationNode
                ], funcTable)
            )
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));
        funcTable.TryAddVariable(new VariableSymbol("x", variableDeclarationNode));

        Assert.That(tree, Is.EqualTo(expected));
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

        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
        var variableDeclarationNode = new VariableDeclarationNode(
            "x",
            "bool",
            new BinaryExpressionNode(
                BinaryExpressionKind.GreaterThan,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2),
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], "void", new BlockStatementNode([variableDeclarationNode], funcTable)
            )
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));
        funcTable.TryAddVariable(new VariableSymbol("x", variableDeclarationNode));

        Assert.That(tree, Is.EqualTo(expected));
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

        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
        var variableDeclarationNode = new VariableDeclarationNode(
            "x",
            "bool",
            new BinaryExpressionNode(
                BinaryExpressionKind.GreaterThanOrEqual,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2),
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2)
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main", [], "void", new BlockStatementNode([variableDeclarationNode], funcTable)
            )
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));
        funcTable.TryAddVariable(new VariableSymbol("x", variableDeclarationNode));

        Assert.That(tree, Is.EqualTo(expected));
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

        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
        var variableDeclarationNode = new VariableDeclarationNode(
            "x",
            "i32",
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
                "main", [], "void", new BlockStatementNode([variableDeclarationNode], funcTable)
            )
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));
        funcTable.TryAddVariable(new VariableSymbol("x", variableDeclarationNode));

        Assert.That(tree, Is.EqualTo(expected));
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

        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
        var variableDeclarationNode = new VariableDeclarationNode(
            "x",
            "i32",
            new BinaryExpressionNode(
                BinaryExpressionKind.Multiplication,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 2),
                new VariableExpressionNode("y")
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [new FunctionParameterNode("y", "i32")],
                "void",
                new BlockStatementNode([variableDeclarationNode], funcTable)
            )
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));
        funcTable.TryAddVariable(new VariableSymbol("y", expected.Functions[0].Parameters[0]));
        funcTable.TryAddVariable(new VariableSymbol("x", variableDeclarationNode));

        Assert.That(tree, Is.EqualTo(expected));
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

        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
        var variableDeclarationNode = new VariableDeclarationNode(
            "x",
            "i32",
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
                "main", [], "void", new BlockStatementNode([variableDeclarationNode], funcTable)
            )
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));
        funcTable.TryAddVariable(new VariableSymbol("x", variableDeclarationNode));

        Assert.That(tree, Is.EqualTo(expected));
    }
}