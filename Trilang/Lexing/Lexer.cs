namespace Trilang.Lexing;

public class Lexer
{
    public IReadOnlyList<Token> Tokenize(string code)
    {
        var span = code.AsSpan();
        var tokens = new List<Token>();

        while (span.Length > 0)
        {
            var c = span[0];
            if (char.IsWhiteSpace(c) || c is '\n' or '\r')
            {
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
                    "true" => Token.Create(TokenKind.True),
                    "false" => Token.Create(TokenKind.False),
                    "for" => Token.Create(TokenKind.For),
                    "while" => Token.Create(TokenKind.While),
                    "break" => Token.Create(TokenKind.Break),
                    "continue" => Token.Create(TokenKind.Continue),
                    "public" => Token.Create(TokenKind.Public),
                    "private" => Token.Create(TokenKind.Private),
                    "type" => Token.Create(TokenKind.Type),
                    "this" => Token.Create(TokenKind.This),
                    "constructor" => Token.Create(TokenKind.Constructor),

                    _ => Token.CreateId(id.ToString()),
                });

                span = span[length..];
                continue;
            }

            if (c is '\'')
            {
                span = span[1..];

                var length = span.IndexOfAny('\'', '\n');
                if (length == -1)
                    throw new Exception("Unterminated string");

                var str = span[..length].ToString();
                tokens.Add(Token.CreateChar(str));

                span = span[length..];

                if (span.Length == 0 || span[0] != '\'')
                    throw new Exception("Unterminated string");

                span = span[1..];
                continue;
            }

            if (c is '"')
            {
                span = span[1..];

                var length = span.IndexOfAny('"', '\n');
                if (length == -1)
                    throw new Exception("Unterminated string");

                var str = span[..length].ToString();
                tokens.Add(Token.CreateString(str));

                span = span[length..];

                if (span.Length == 0 || span[0] != '"')
                    throw new Exception("Unterminated string");

                span = span[1..];
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

                ('+', '=') => (Token.Create(TokenKind.PlusEqual), 2),
                ('+', _) => (Token.Create(TokenKind.Plus), 1),
                ('-', '=') => (Token.Create(TokenKind.MinusEqual), 2),
                ('-', _) => (Token.Create(TokenKind.Minus), 1),
                ('*', '=') => (Token.Create(TokenKind.AsteriskEqual), 2),
                ('*', _) => (Token.Create(TokenKind.Asterisk), 1),
                ('/', '=') => (Token.Create(TokenKind.SlashEqual), 2),
                ('/', _) => (Token.Create(TokenKind.Slash), 1),

                ('=', '>') => (Token.Create(TokenKind.EqualGreater), 2),

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
                ('&', '=') => (Token.Create(TokenKind.AmpersandEqual), 2),
                ('&', _) => (Token.Create(TokenKind.Ampersand), 1),
                ('|', '=') => (Token.Create(TokenKind.PipeEqual), 2),
                ('|', _) => (Token.Create(TokenKind.Pipe), 1),
                ('^', '=') => (Token.Create(TokenKind.CaretEqual), 2),
                ('^', _) => (Token.Create(TokenKind.Caret), 1),
                ('~', _) => (Token.Create(TokenKind.Tilde), 1),
                ('%', '=') => (Token.Create(TokenKind.PercentEqual), 2),
                ('%', _) => (Token.Create(TokenKind.Percent), 1),

                ('.', _) => (Token.Create(TokenKind.Dot), 1),

                _ => throw new Exception($"Unexpected character '{c}'")
            };
            tokens.Add(token);
            span = span[size..];
        }

        tokens.Add(Token.CreateEof());

        return tokens;
    }
}