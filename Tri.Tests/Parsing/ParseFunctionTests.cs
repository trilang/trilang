using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseFunctionTests
{
    private static SyntaxTree Parse(string code)
    {
        var diagnostics = new DiagnosticCollection();
        var lexer = new Lexer();
        var tokens = lexer.Tokenize(code, new LexerOptions(diagnostics.Lexer));

        return new Parser().Parse(tokens);
    }

    [Test]
    public void ParseMissingIdFunctionTest()
    {
        Assert.That(
            () => Parse("public;"),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected a type declaration."));
    }

    [Test]
    public void ParseMissingOpenParenFunctionTest()
    {
        Assert.That(
            () => Parse("public test { }"),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected an open parenthesis."));
    }

    [Test]
    public void ParseMissingParamColonFunctionTest()
    {
        Assert.That(
            () => Parse("public test(x i32) { }"),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected a colon."));
    }

    [Test]
    public void ParseMissingParamTypeFunctionTest()
    {
        Assert.That(
            () => Parse("public test(x: ) { }"),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected a type."));
    }

    [Test]
    public void ParseMissingCommaFunctionTest()
    {
        Assert.That(
            () => Parse("public test(x: i32 y: i32): void { }"),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected an close parenthesis."));
    }

    [Test]
    public void ParseMissingCloseParenFunctionTest()
    {
        Assert.That(
            () => Parse("public test( { }"),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected an close parenthesis."));
    }

    [Test]
    public void ParseMissingReturnColonFunctionTest()
    {
        Assert.That(
            () => Parse("public test()"),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected a colon."));
    }

    [Test]
    public void ParseMissingReturnTypeFunctionTest()
    {
        Assert.That(
            () => Parse("public test():"),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected a function return type."));
    }

    [Test]
    public void ParseEmptyFunctionTest()
    {
        var tree = Parse("public test(): void { }");

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
        var tree = Parse("public test(x: i32): void { }");

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
        var tree = Parse("public test(x: i32, y: i32, z: i32): void { }");

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
        var tree = Parse("public test(x: i32[]): void { }");

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