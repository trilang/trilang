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

        Assert.Throws<ParseException>(() => parser.Parse("public test(x i32) { }"));
    }

    [Test]
    public void ParseMissingParamTypeFunctionTest()
    {
        var parser = new Parser();

        Assert.Throws<ParseException>(() => parser.Parse("public test(x: ) { }"));
    }

    [Test]
    public void ParseMissingCommaFunctionTest()
    {
        var parser = new Parser();

        Assert.Throws<ParseException>(() => parser.Parse(
            "public test(x: i32 y: i32): void { }"));
    }

    [Test]
    public void ParseMissingCloseParenFunctionTest()
    {
        var parser = new Parser();

        Assert.Throws<ParseException>(() => parser.Parse("public test( { }"));
    }

    [Test]
    public void ParseMissingReturnColonFunctionTest()
    {
        var parser = new Parser();

        Assert.Throws<ParseException>(() => parser.Parse("public test()"));
    }

    [Test]
    public void ParseMissingReturnTypeFunctionTest()
    {
        var parser = new Parser();

        Assert.Throws<ParseException>(() => parser.Parse("public test():"));
    }

    [Test]
    public void ParseEmptyFunctionTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public test(): void { }");

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(23, 1, 24)),
                AccessModifier.Public,
                "test",
                [],
                new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(23, 1, 24)))
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseSingleParameterFunctionTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public test(x: i32): void { }");

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(29, 1, 30)),
                AccessModifier.Public,
                "test",
                [
                    new ParameterNode(new SourceSpan(new SourcePosition(12, 1, 13), new SourcePosition(18, 1, 19)), "x", new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(18, 1, 19)), "i32")),
                ],
                new TypeNode(new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(25, 1, 26)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(26, 1, 27), new SourcePosition(29, 1, 30)))
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseMultipleParametersFunctionTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public test(x: i32, y: i32, z: i32): void { }");

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(45, 1, 46)),
                AccessModifier.Public,
                "test",
                [
                    new ParameterNode(new SourceSpan(new SourcePosition(12, 1, 13), new SourcePosition(18, 1, 19)), "x", new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(18, 1, 19)), "i32")),
                    new ParameterNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(26, 1, 27)), "y", new TypeNode(new SourceSpan(new SourcePosition(23, 1, 24), new SourcePosition(26, 1, 27)), "i32")),
                    new ParameterNode(new SourceSpan(new SourcePosition(28, 1, 29), new SourcePosition(34, 1, 35)), "z", new TypeNode(new SourceSpan(new SourcePosition(31, 1, 32), new SourcePosition(34, 1, 35)), "i32")),
                ],
                new TypeNode(new SourceSpan(new SourcePosition(37, 1, 38), new SourcePosition(41, 1, 42)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(42, 1, 43), new SourcePosition(45, 1, 46)))
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseArrayTypeTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public test(x: i32[]): void { }");

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(31, 1, 32)),
                AccessModifier.Public,
                "test",
                [new ParameterNode(new SourceSpan(new SourcePosition(12, 1, 13), new SourcePosition(20, 1, 21)), "x", new ArrayTypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(20, 1, 21)), new TypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(18, 1, 19)), "i32")))],
                new TypeNode(new SourceSpan(new SourcePosition(23, 1, 24), new SourcePosition(27, 1, 28)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(28, 1, 29), new SourcePosition(31, 1, 32)))
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }
}