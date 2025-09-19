using Trilang;
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
            Token.CreateEof(new SourcePosition(2, 3, 1).ToSpan()),
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
            Token.CreateEof(new SourcePosition(2, 1, 3).ToSpan()),
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
            Token.CreateInteger(
                new SourceSpan(new SourcePosition(), new SourcePosition(code.Length, 1, code.Length + 1)),
                number
            ),
            Token.CreateEof(new SourcePosition(code.Length, 1, code.Length + 1).ToSpan()),
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
            Token.CreateFloat(
                new SourceSpan(new SourcePosition(), new SourcePosition(code.Length, 1, code.Length + 1)),
                number
            ),
            Token.CreateEof(new SourcePosition(code.Length, 1, code.Length + 1).ToSpan()),
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
            Token.CreateFloat(new SourceSpan(new SourcePosition(), new SourcePosition(3, 1, 4)), 1.2),
            Token.Create(new SourceSpan(new SourcePosition(3, 1, 4), new SourcePosition(4, 1, 5)), TokenKind.Dot),
            Token.CreateInteger(new SourceSpan(new SourcePosition(4, 1, 5), new SourcePosition(5, 1, 6)), 3),
            Token.CreateEof(new SourcePosition(5, 1, 6).ToSpan()),
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
            Token.Create(new SourceSpan(new SourcePosition(), new SourcePosition(1, 1, 2)), TokenKind.Dot),
            Token.CreateInteger(new SourceSpan(new SourcePosition(1, 1, 2), new SourcePosition(2, 1, 3)), 3),
            Token.CreateEof(new SourcePosition(2, 1, 3).ToSpan()),
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
            Token.CreateInteger(new SourceSpan(new SourcePosition(), new SourcePosition(1, 1, 2)), 1),
            Token.Create(new SourceSpan(new SourcePosition(1, 1, 2), new SourcePosition(2, 1, 3)), TokenKind.Dot),
            Token.CreateId(new SourceSpan(new SourcePosition(2, 1, 3), new SourcePosition(10, 1, 11)), "toString"),
            Token.Create(new SourceSpan(new SourcePosition(10, 1, 11), new SourcePosition(11, 1, 12)), TokenKind.OpenParenthesis),
            Token.Create(new SourceSpan(new SourcePosition(11, 1, 12), new SourcePosition(12, 1, 13)), TokenKind.CloseParenthesis),
            Token.CreateEof(new SourcePosition(12, 1, 13).ToSpan()),
        };

        Assert.That(tokens, Is.EqualTo(expected));
    }

    [Test]
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
    [TestCase("internal", TokenKind.Internal)]
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
            Token.Create(
                new SourceSpan(new SourcePosition(), new SourcePosition(code.Length, 1, code.Length + 1)),
                kind
            ),
            Token.CreateEof(new SourcePosition(code.Length, 1, code.Length + 1).ToSpan()),
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
            Token.CreateId(
                new SourceSpan(new SourcePosition(), new SourcePosition(code.Length, 1, code.Length + 1)),
                code),
            Token.CreateEof(new SourcePosition(code.Length, 1, code.Length + 1).ToSpan()),
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
            Token.Create(
                new SourceSpan(new SourcePosition(), new SourcePosition(code.Length, 1, code.Length + 1)),
                kind
            ),
            Token.CreateEof(new SourcePosition(code.Length, 1, code.Length + 1).ToSpan()),
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
            Token.CreateChar(
                new SourceSpan(new SourcePosition(), new SourcePosition(code.Length + 2, 1, code.Length + 3)),
                code
            ),
            Token.CreateEof(new SourcePosition(code.Length + 2, 1, code.Length + 3).ToSpan()),
        };

        Assert.That(tokens, Is.EqualTo(expected));
    }

    [Test]
    public void TokenizeMultilineCharTest()
    {
        var lexer = new Lexer();
        var tokens = lexer.Tokenize("'line\nline\nline'");
        var expected = new[]
        {
            Token.CreateChar(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(16, 3, 6)),
                "line\nline\nline"
            ),
            Token.CreateEof(new SourcePosition(16, 3, 6).ToSpan()),
        };

        Assert.That(tokens, Is.EqualTo(expected));
    }

    [Test]
    public void TokenizeMultilineCharWithoutEndQuoteTest()
    {
        var lexer = new Lexer();
        var tokens = lexer.Tokenize("'line\nline\nline");
        var expected = new[]
        {
            Token.CreateChar(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(15, 3, 5)),
                "line\nline\nline"
            ),
            Token.CreateEof(new SourcePosition(15, 3, 5).ToSpan()),
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
            Token.CreateString(
                new SourceSpan(new SourcePosition(), new SourcePosition(code.Length + 2, 1, code.Length + 3)),
                code
            ),
            Token.CreateEof(new SourcePosition(code.Length + 2, 1, code.Length + 3).ToSpan()),
        };

        Assert.That(tokens, Is.EqualTo(expected));
    }

    [Test]
    public void TokenizeMultilineStringTest()
    {
        var lexer = new Lexer();
        var tokens = lexer.Tokenize("\"line\nline\nline\"");
        var expected = new[]
        {
            Token.CreateString(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(16, 3, 6)),
                "line\nline\nline"
            ),
            Token.CreateEof(new SourcePosition(16, 3, 6).ToSpan()),
        };

        Assert.That(tokens, Is.EqualTo(expected));
    }

    [Test]
    public void TokenizeMultilineStringWithoutEndQuoteTest()
    {
        var lexer = new Lexer();
        var tokens = lexer.Tokenize("\"line\nline\nline");
        var expected = new[]
        {
            Token.CreateString(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(15, 3, 5)),
                "line\nline\nline"
            ),
            Token.CreateEof(new SourcePosition(15, 3, 5).ToSpan()),
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
            public main(): void {}
            """);
        var expected = new[]
        {
            Token.Create(new SourceSpan(new SourcePosition(11, 2, 1), new SourcePosition(17, 2, 7)), TokenKind.Public),
            Token.CreateId(new SourceSpan(new SourcePosition(18, 2, 8), new SourcePosition(22, 2, 12)), "main"),
            Token.Create(new SourceSpan(new SourcePosition(22, 2, 12), new SourcePosition(23, 2, 13)), TokenKind.OpenParenthesis),
            Token.Create(new SourceSpan(new SourcePosition(23, 2, 13), new SourcePosition(24, 2, 14)), TokenKind.CloseParenthesis),
            Token.Create(new SourceSpan(new SourcePosition(24, 2, 14), new SourcePosition(25, 2, 15)), TokenKind.Colon),
            Token.CreateId(new SourceSpan(new SourcePosition(26, 2, 16), new SourcePosition(30, 2, 20)), "void"),
            Token.Create(new SourceSpan(new SourcePosition(31, 2, 21), new SourcePosition(32, 2, 22)), TokenKind.OpenBrace),
            Token.Create(new SourceSpan(new SourcePosition(32, 2, 22), new SourcePosition(33, 2, 23)), TokenKind.CloseBrace),
            Token.CreateEof(new SourcePosition(33, 2, 23).ToSpan()),
        };

        Assert.That(tokens, Is.EqualTo(expected));
    }

    [Test]
    public void TokenizeCommentAtTheEndTest()
    {
        var lexer = new Lexer();
        var tokens = lexer.Tokenize(
            """
            public main(): void {}
            // comment
            """);
        var expected = new[]
        {
            Token.Create(new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(6, 1, 7)), TokenKind.Public),
            Token.CreateId(new SourceSpan(new SourcePosition(7, 1, 8), new SourcePosition(11, 1, 12)), "main"),
            Token.Create(new SourceSpan(new SourcePosition(11, 1, 12), new SourcePosition(12, 1, 13)), TokenKind.OpenParenthesis),
            Token.Create(new SourceSpan(new SourcePosition(12, 1, 13), new SourcePosition(13, 1, 14)), TokenKind.CloseParenthesis),
            Token.Create(new SourceSpan(new SourcePosition(13, 1, 14), new SourcePosition(14, 1, 15)), TokenKind.Colon),
            Token.CreateId(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
            Token.Create(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(21, 1, 22)), TokenKind.OpenBrace),
            Token.Create(new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(22, 1, 23)), TokenKind.CloseBrace),
            Token.CreateEof(new SourcePosition(33, 2, 11).ToSpan()),
        };

        Assert.That(tokens, Is.EqualTo(expected));
    }
}