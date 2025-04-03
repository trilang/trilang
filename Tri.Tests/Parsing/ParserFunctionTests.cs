using Trilang.Parsing;
using Trilang.Parsing.Ast;

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

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "test",
                [],
                TypeNode.Create("void"),
                new BlockStatementNode())
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseSingleParameterFunctionTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("function test(x: i32): void { }");

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "test",
                [
                    new FunctionParameterNode("x", TypeNode.Create("i32")),
                ],
                TypeNode.Create("void"),
                new BlockStatementNode())
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseMultipleParametersFunctionTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("function test(x: i32, y: i32, z: i32): void { }");

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "test",
                [
                    new FunctionParameterNode("x", TypeNode.Create("i32")),
                    new FunctionParameterNode("y", TypeNode.Create("i32")),
                    new FunctionParameterNode("z", TypeNode.Create("i32")),
                ],
                TypeNode.Create("void"),
                new BlockStatementNode())
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseArrayTypeTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("function test(x: i32[]): void { }");

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "test",
                [new FunctionParameterNode("x", TypeNode.Array("i32"))],
                TypeNode.Create("void"),
                new BlockStatementNode())
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }
}