namespace Trilang.Lexing;

public class Lexer
{
    public IEnumerable<Token> Tokenize(string code)
    {
        var span = code.AsSpan();
        var tokens = new List<Token>();

        while (span.Length > 0)
        {
            var c = span[0];
            if (c == '\n')
            {
                tokens.Add(Token.CreateNewLine());
                span = span[1..];
                continue;
            }

            if (c == '\r')
            {
                tokens.Add(Token.CreateCarriageReturn());
                span = span[1..];
                continue;
            }

            if (char.IsWhiteSpace(c))
            {
                tokens.Add(Token.CreateWhiteSpace());
                span = span[1..];
                continue;
            }

            if (char.IsDigit(c))
            {
                var number = 0;
                while (span.Length > 0 && char.IsDigit(span[0]))
                {
                    number = number * 10 + (span[0] - '0');
                    span = span[1..];
                }

                tokens.Add(Token.CreateNumber(number));
                continue;
            }

            if (char.IsLetter(c))
            {
                var length = 1;
                while (length < span.Length && char.IsLetterOrDigit(span[length]))
                    length++;

                var id = span[..length];
                tokens.Add(id switch
                {
                    "function" => Token.Create(TokenKind.Function),
                    "var" => Token.Create(TokenKind.Var),
                    "if" => Token.Create(TokenKind.If),
                    "else" => Token.Create(TokenKind.Else),
                    "external" => Token.Create(TokenKind.External),
                    "return" => Token.Create(TokenKind.Return),

                    _ => Token.CreateId(id.ToString()),
                });

                span = span[length..];
                continue;
            }

            var next = span.Length > 1 ? span[1] : '\0';
            var (token, size) = (c, next) switch
            {
                ('(', _) => (Token.Create(TokenKind.OpenParenthesis), 1),
                (')', _) => (Token.Create(TokenKind.CloseParenthesis), 1),
                ('{', _) => (Token.Create(TokenKind.OpenBrace), 1),
                ('}', _) => (Token.Create(TokenKind.CloseBrace), 1),
                ('[', _) => (Token.Create(TokenKind.OpenBracket), 1),
                (']', _) => (Token.Create(TokenKind.CloseBracket), 1),

                (':', _) => (Token.Create(TokenKind.Colon), 1),
                (';', _) => (Token.Create(TokenKind.SemiColon), 1),
                (',', _) => (Token.Create(TokenKind.Comma), 1),

                ('+', _) => (Token.Create(TokenKind.Plus), 1),
                ('-', _) => (Token.Create(TokenKind.Minus), 1),
                ('*', _) => (Token.Create(TokenKind.Asterisk), 1),
                ('/', _) => (Token.Create(TokenKind.Slash), 1),

                ('=', '=') => (Token.Create(TokenKind.EqualEqual), 2),
                ('=', _) => (Token.Create(TokenKind.Equal), 1),
                ('!', '=') => (Token.Create(TokenKind.ExclamationEqual), 2),
                ('>', '=') => (Token.Create(TokenKind.GreaterEqual), 2),
                ('<', '=') => (Token.Create(TokenKind.LessEqual), 2),
                ('<', _) => (Token.Create(TokenKind.Less), 1),
                ('>', _) => (Token.Create(TokenKind.Greater), 1),
                ('!', _) => (Token.Create(TokenKind.Exclamation), 1),

                ('&', '&') => (Token.Create(TokenKind.AmpersandAmpersand), 2),
                ('|', '|') => (Token.Create(TokenKind.PipePipe), 2),
                ('&', _) => (Token.Create(TokenKind.Ampersand), 1),
                ('|', _) => (Token.Create(TokenKind.Pipe), 1),

                _ => throw new Exception($"Unexpected character '{c}'")
            };
            tokens.Add(token);
            span = span[size..];
        }

        tokens.Add(Token.CreateEof());

        return tokens;
    }
}