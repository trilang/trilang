using Trilang;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseFunctionTests
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
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(25, 1, 26)),
                "test",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(25, 1, 26)))
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseSingleParameterFunctionTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("function test(x: i32): void { }");

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(31, 1, 32)),
                "test",
                [
                    new ParameterNode(new SourceSpan(new SourcePosition(14, 1, 15), new SourcePosition(20, 1, 21)), "x", new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(20, 1, 21)), "i32")),
                ],
                new TypeNode(new SourceSpan(new SourcePosition(23, 1, 24), new SourcePosition(27, 1, 28)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(28, 1, 29), new SourcePosition(31, 1, 32)))
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseMultipleParametersFunctionTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("function test(x: i32, y: i32, z: i32): void { }");

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(47, 1, 48)),
                "test",
                [
                    new ParameterNode(new SourceSpan(new SourcePosition(14, 1, 15), new SourcePosition(20, 1, 21)), "x", new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(20, 1, 21)), "i32")),
                    new ParameterNode(new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(28, 1, 29)), "y", new TypeNode(new SourceSpan(new SourcePosition(25, 1, 26), new SourcePosition(28, 1, 29)), "i32")),
                    new ParameterNode(new SourceSpan(new SourcePosition(30, 1, 31), new SourcePosition(36, 1, 37)), "z", new TypeNode(new SourceSpan(new SourcePosition(33, 1, 34), new SourcePosition(36, 1, 37)), "i32")),
                ],
                new TypeNode(new SourceSpan(new SourcePosition(39, 1, 40), new SourcePosition(43, 1, 44)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(44, 1, 45), new SourcePosition(47, 1, 48)))
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseArrayTypeTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("function test(x: i32[]): void { }");

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(33, 1, 34)),
                "test",
                [new ParameterNode(new SourceSpan(new SourcePosition(14, 1, 15), new SourcePosition(22, 1, 23)), "x", new ArrayTypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(22, 1, 23)), new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(20, 1, 21)), "i32")))],
                new TypeNode(new SourceSpan(new SourcePosition(25, 1, 26), new SourcePosition(29, 1, 30)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(30, 1, 31), new SourcePosition(33, 1, 34)))
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }
}