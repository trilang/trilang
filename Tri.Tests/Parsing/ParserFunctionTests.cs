using Trilang.Parsing;
using Trilang.Parsing.Nodes;
using Trilang.Symbols;

namespace Tri.Tests.Parsing;

public class ParserFunctionTests
{
    [Test]
    public void ParseMissingIdFunctionTest()
    {
        var parser = new Parser();

        Assert.Throws<ParseException>(() => parser.Parse("function;"));
    }

    [Test]
    public void ParseMissingOpenParenFunctionTest()
    {
        var parser = new Parser();

        Assert.Throws<ParseException>(() => parser.Parse("function test { }"));
    }

    [Test]
    public void ParseMissingParamColonFunctionTest()
    {
        var parser = new Parser();

        Assert.Throws<ParseException>(() => parser.Parse("function test(x i32) { }"));
    }

    [Test]
    public void ParseMissingParamTypeFunctionTest()
    {
        var parser = new Parser();

        Assert.Throws<ParseException>(() => parser.Parse("function test(x: ) { }"));
    }

    [Test]
    public void ParseMissingCommaFunctionTest()
    {
        var parser = new Parser();

        Assert.Throws<ParseException>(() => parser.Parse(
            "function test(x: i32 y: i32): void { }"));
    }

    [Test]
    public void ParseMissingCloseParenFunctionTest()
    {
        var parser = new Parser();

        Assert.Throws<ParseException>(() => parser.Parse("function test( { }"));
    }

    [Test]
    public void ParseMissingReturnColonFunctionTest()
    {
        var parser = new Parser();

        Assert.Throws<ParseException>(() => parser.Parse("function test()"));
    }

    [Test]
    public void ParseMissingReturnTypeFunctionTest()
    {
        var parser = new Parser();

        Assert.Throws<ParseException>(() => parser.Parse("function test():"));
    }

    [Test]
    public void ParseEmptyFunctionTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("function test(): void { }");
        var rootTable = new SymbolTable();
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "test",
                [],
                "void",
                new BlockStatementNode(rootTable.CreateChild()))
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseSingleParameterFunctionTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("function test(x: i32): void { }");
        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "test",
                [
                    new FunctionParameterNode("x", "i32"),
                ],
                "void",
                new BlockStatementNode(funcTable))
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));
        funcTable.TryAddVariable(new VariableSymbol("x", expected.Functions[0].Parameters[0]));

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseMultipleParametersFunctionTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("function test(x: i32, y: i32, z: i32): void { }");
        var rootTable = new SymbolTable();
        var funcTable = rootTable.CreateChild();
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "test",
                [
                    new FunctionParameterNode("x", "i32"),
                    new FunctionParameterNode("y", "i32"),
                    new FunctionParameterNode("z", "i32"),
                ],
                "void",
                new BlockStatementNode(funcTable))
        ], rootTable);
        rootTable.TryAddFunction(new FunctionSymbol(expected.Functions[0]));
        funcTable.TryAddVariable(new VariableSymbol("x", expected.Functions[0].Parameters[0]));
        funcTable.TryAddVariable(new VariableSymbol("y", expected.Functions[0].Parameters[1]));
        funcTable.TryAddVariable(new VariableSymbol("z", expected.Functions[0].Parameters[2]));

        Assert.That(tree, Is.EqualTo(expected));
    }
}