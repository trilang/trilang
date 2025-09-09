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
            Token.CreateInteger(number),
            Token.CreateEof(),
        };

        Assert.That(tokens, Is.EqualTo(expected));
    }

    [Test]
    [TestCase("1.0", 1.0)]
    [TestCase("123.456", 123.456)]
    [TestCase("0.5", 0.5)]
    [TestCase("3.14159", 3.14159)]
    [TestCase("0.0", 0.0)]
    public void TokenizeFloatNumberTest(string code, double number)
    {
        var lexer = new Lexer();
        var tokens = lexer.Tokenize(code);
        var expected = new[]
        {
            Token.CreateFloat(number),
            Token.CreateEof(),
        };

        Assert.That(tokens, Is.EqualTo(expected));
    }

    [Test]
    public void TokenizeFloatNumberWithDoubleDotTest()
    {
        var lexer = new Lexer();
        var tokens = lexer.Tokenize("1.2.3");
        var expected = new[]
        {
            Token.CreateFloat(1.2),
            Token.Create(TokenKind.Dot),
            Token.CreateInteger(3),
            Token.CreateEof(),
        };

        Assert.That(tokens, Is.EqualTo(expected));
    }

    [Test]
    public void TokenizeFloatNumberWithLeadingDotTest()
    {
        var lexer = new Lexer();
        var tokens = lexer.Tokenize(".3");
        var expected = new[]
        {
            Token.Create(TokenKind.Dot),
            Token.CreateInteger(3),
            Token.CreateEof(),
        };

        Assert.That(tokens, Is.EqualTo(expected));
    }

    [Test]
    public void TokenizeMethodAfterNumberTest()
    {
        var lexer = new Lexer();
        var tokens = lexer.Tokenize("1.toString()");
        var expected = new[]
        {
            Token.CreateInteger(1),
            Token.Create(TokenKind.Dot),
            Token.CreateId("toString"),
            Token.Create(TokenKind.OpenParenthesis),
            Token.Create(TokenKind.CloseParenthesis),
            Token.CreateEof(),
        };

        Assert.That(tokens, Is.EqualTo(expected));
    }

    [Test]
    [TestCase("function", TokenKind.Function)]
    [TestCase("var", TokenKind.Var)]
    [TestCase("if", TokenKind.If)]
    [TestCase("else", TokenKind.Else)]
    [TestCase("endif", TokenKind.EndIf)]
    [TestCase("external", TokenKind.External)]
    [TestCase("return", TokenKind.Return)]
    [TestCase("true", TokenKind.True)]
    [TestCase("false", TokenKind.False)]
    [TestCase("for", TokenKind.For)]
    [TestCase("while", TokenKind.While)]
    [TestCase("break", TokenKind.Break)]
    [TestCase("continue", TokenKind.Continue)]
    [TestCase("public", TokenKind.Public)]
    [TestCase("private", TokenKind.Private)]
    [TestCase("type", TokenKind.Type)]
    [TestCase("constructor", TokenKind.Constructor)]
    [TestCase("new", TokenKind.New)]
    [TestCase("null", TokenKind.Null)]
    [TestCase("get", TokenKind.Get)]
    [TestCase("set", TokenKind.Set)]
    [TestCase("static", TokenKind.Static)]
    [TestCase("is", TokenKind.Is)]
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
    [TestCase("+=", TokenKind.PlusEqual)]
    [TestCase("-", TokenKind.Minus)]
    [TestCase("-=", TokenKind.MinusEqual)]
    [TestCase("*", TokenKind.Asterisk)]
    [TestCase("*=", TokenKind.AsteriskEqual)]
    [TestCase("/", TokenKind.Slash)]
    [TestCase("/=", TokenKind.SlashEqual)]
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
    [TestCase("&=", TokenKind.AmpersandEqual)]
    [TestCase("|", TokenKind.Pipe)]
    [TestCase("|=", TokenKind.PipeEqual)]
    [TestCase("^", TokenKind.Caret)]
    [TestCase("^=", TokenKind.CaretEqual)]
    [TestCase("~", TokenKind.Tilde)]
    [TestCase("=>", TokenKind.EqualGreater)]
    [TestCase(".", TokenKind.Dot)]
    [TestCase("#", TokenKind.Hash)]
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
    [TestCase("1")]
    [TestCase("xxx")]
    public void TokenizeCharTest(string code)
    {
        var lexer = new Lexer();
        var tokens = lexer.Tokenize($"'{code}'");
        var expected = new[]
        {
            Token.CreateChar(code),
            Token.CreateEof(),
        };

        Assert.That(tokens, Is.EqualTo(expected));
    }

    [Test]
    [TestCase("1")]
    [TestCase("xxx")]
    public void TokenizeStringTest(string code)
    {
        var lexer = new Lexer();
        var tokens = lexer.Tokenize($"\"{code}\"");
        var expected = new[]
        {
            Token.CreateString(code),
            Token.CreateEof(),
        };

        Assert.That(tokens, Is.EqualTo(expected));
    }

    [Test]
    [TestCase("$")]
    public void TokenizeUnsupportedCharacterTest(string code)
    {
        var lexer = new Lexer();

        Assert.Throws<LexerException>(() => lexer.Tokenize(code));
    }

    [Test]
    public void TokenizeCommentTest()
    {
        var lexer = new Lexer();
        var tokens = lexer.Tokenize(
            """
            // comment
            function main(): void {}
            """);
        var expected = new[]
        {
            Token.Create(TokenKind.Function),
            Token.CreateId("main"),
            Token.Create(TokenKind.OpenParenthesis),
            Token.Create(TokenKind.CloseParenthesis),
            Token.Create(TokenKind.Colon),
            Token.CreateId("void"),
            Token.Create(TokenKind.OpenBrace),
            Token.Create(TokenKind.CloseBrace),
            Token.CreateEof(),
        };

        Assert.That(tokens, Is.EqualTo(expected));
    }

    [Test]
    public void TokenizeCommentAtTheEndTest()
    {
        var lexer = new Lexer();
        var tokens = lexer.Tokenize(
            """
            function main(): void {}
            // comment
            """);
        var expected = new[]
        {
            Token.Create(TokenKind.Function),
            Token.CreateId("main"),
            Token.Create(TokenKind.OpenParenthesis),
            Token.Create(TokenKind.CloseParenthesis),
            Token.Create(TokenKind.Colon),
            Token.CreateId("void"),
            Token.Create(TokenKind.OpenBrace),
            Token.Create(TokenKind.CloseBrace),
            Token.CreateEof(),
        };

        Assert.That(tokens, Is.EqualTo(expected));
    }
}