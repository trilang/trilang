using Trilang.Parsing;
using Trilang.Parsing.Nodes;
using Trilang.Symbols;

namespace Tri.Tests.Parsing;

public class ParseCallExpressionTests
{
    [Test]
    public void ParseCallStatementTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                print("Hello, World!");
            }
            """);
        var rootTable = new SymbolTable();
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                "void",
                new BlockStatementNode([
                    new ExpressionStatementNode(
                        new CallExpressionNode("print", [
                            new LiteralExpressionNode(LiteralExpressionKind.String, "Hello, World!")
                        ])
                    )
                ], rootTable.CreateChild())
            )
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseCallStatementMultipleParamsTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                sum(1, 2, 3);
            }
            """);
        var rootTable = new SymbolTable();
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                "void",
                new BlockStatementNode([
                    new ExpressionStatementNode(
                        new CallExpressionNode("sum", [
                            new LiteralExpressionNode(LiteralExpressionKind.Number, 1),
                            new LiteralExpressionNode(LiteralExpressionKind.Number, 2),
                            new LiteralExpressionNode(LiteralExpressionKind.Number, 3),
                        ])
                    )
                ], rootTable.CreateChild())
            )
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseCallExpressionMultipleParamsTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                var x: i32 = 1 + sum(1, 2, 3);
            }
            """);
        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
        var variableDeclarationNode = new VariableDeclarationNode(
            "x",
            "i32",
            new BinaryExpressionNode(
                BinaryExpressionKind.Addition,
                new LiteralExpressionNode(LiteralExpressionKind.Number, 1),
                new CallExpressionNode("sum", [
                    new LiteralExpressionNode(LiteralExpressionKind.Number, 1),
                    new LiteralExpressionNode(LiteralExpressionKind.Number, 2),
                    new LiteralExpressionNode(LiteralExpressionKind.Number, 3),
                ])
            )
        );
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                "void",
                new BlockStatementNode([variableDeclarationNode], funcTable)
            )
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));
        funcTable.TryAddVariable(new VariableSymbol("x", variableDeclarationNode));

        Assert.That(tree, Is.EqualTo(expected));
    }
}