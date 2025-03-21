using Trilang.Lexing;

namespace Tri.Tests;

public class LexerTests
{
    [Test]
    public void TokenizeNewLineTest()
    {
        var lexer = new Lexer();
        var tokens = lexer.Tokenize("\n\n");
        var expected = new[]
        {
            Token.CreateNewLine(),
            Token.CreateNewLine(),
            Token.CreateEof(),
        };

        Assert.That(tokens, Is.EqualTo(expected));
    }

    [Test]
    public void TokenizeCarriageReturnTest()
    {
        var lexer = new Lexer();
        var tokens = lexer.Tokenize("\r\r");
        var expected = new[]
        {
            Token.CreateCarriageReturn(),
            Token.CreateCarriageReturn(),
            Token.CreateEof(),
        };

        Assert.That(tokens, Is.EqualTo(expected));
    }

    [Test]
    public void TokenizeWhiteSpaceTest()
    {
        var lexer = new Lexer();
        var tokens = lexer.Tokenize("  ");
        var expected = new[]
        {
            Token.CreateWhiteSpace(),
            Token.CreateWhiteSpace(),
            Token.CreateEof(),
        };

        Assert.That(tokens, Is.EqualTo(expected));
    }

    [Test]
    [TestCase("1", 1)]
    [TestCase("123", 123)]
    [TestCase("0123", 123)]
    public void TokenizeNumberTest(string code, int number)
    {
        var lexer = new Lexer();
        var tokens = lexer.Tokenize(code);
        var expected = new[]
        {
            Token.CreateNumber(number),
            Token.CreateEof(),
        };

        Assert.That(tokens, Is.EqualTo(expected));
    }

    [Test]
    [TestCase("function", TokenKind.Function)]
    [TestCase("var", TokenKind.Var)]
    [TestCase("if", TokenKind.If)]
    [TestCase("else", TokenKind.Else)]
    [TestCase("external", TokenKind.External)]
    [TestCase("return", TokenKind.Return)]
    public void TokenizeKeywordTest(string code, TokenKind kind)
    {
        var lexer = new Lexer();
        var tokens = lexer.Tokenize(code);
        var expected = new[]
        {
            Token.Create(kind),
            Token.CreateEof(),
        };

        Assert.That(tokens, Is.EqualTo(expected));
    }

    [Test]
    [TestCase("x")]
    [TestCase("x2")]
    [TestCase("x2dfsf")]
    public void TokenizeIdTest(string code)
    {
        var lexer = new Lexer();
        var tokens = lexer.Tokenize(code);
        var expected = new[]
        {
            Token.CreateId(code),
            Token.CreateEof(),
        };

        Assert.That(tokens, Is.EqualTo(expected));
    }

    [Test]
    [TestCase("(", TokenKind.OpenParenthesis)]
    [TestCase(")", TokenKind.CloseParenthesis)]
    [TestCase("{", TokenKind.OpenBrace)]
    [TestCase("}", TokenKind.CloseBrace)]
    [TestCase("[", TokenKind.OpenBracket)]
    [TestCase("]", TokenKind.CloseBracket)]
    [TestCase(":", TokenKind.Colon)]
    [TestCase(";", TokenKind.SemiColon)]
    [TestCase(",", TokenKind.Comma)]
    [TestCase("+", TokenKind.Plus)]
    [TestCase("-", TokenKind.Minus)]
    [TestCase("*", TokenKind.Asterisk)]
    [TestCase("/", TokenKind.Slash)]
    [TestCase("==", TokenKind.EqualEqual)]
    [TestCase("=", TokenKind.Equal)]
    [TestCase("!=", TokenKind.ExclamationEqual)]
    [TestCase(">=", TokenKind.GreaterEqual)]
    [TestCase("<=", TokenKind.LessEqual)]
    [TestCase(">", TokenKind.Greater)]
    [TestCase("<", TokenKind.Less)]
    [TestCase("!", TokenKind.Exclamation)]
    [TestCase("&&", TokenKind.AmpersandAmpersand)]
    [TestCase("||", TokenKind.PipePipe)]
    [TestCase("&", TokenKind.Ampersand)]
    [TestCase("|", TokenKind.Pipe)]
    public void TokenizeOperatorTest(string code, TokenKind kind)
    {
        var lexer = new Lexer();
        var tokens = lexer.Tokenize(code);
        var expected = new[]
        {
            Token.Create(kind),
            Token.CreateEof(),
        };

        Assert.That(tokens, Is.EqualTo(expected));
    }

    [Test]
    [TestCase("$")]
    public void TokenizeUnsupportedCharacterTest(string code)
    {
        var lexer = new Lexer();

        Assert.Throws<Exception>(() => lexer.Tokenize(code));
    }
}