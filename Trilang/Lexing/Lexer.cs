namespace Trilang.Lexing;

public class Lexer
{
    public IReadOnlyList<Token> Tokenize(string code, LexerOptions options)
    {
        var diagnostics = options.Diagnostics;
        var position = new SourceSpanBuilder();
        var tokens = new List<Token>();

        for (; position.Index < code.Length;)
        {
            var c = code[position.Index];
            var next = code.Length > position.Index + 1 ? code[position.Index + 1] : '\0';

            if (c == '/' && next == '/')
            {
                var length = code.IndexOf('\n', position.Index);
                if (length == -1)
                    length = code.Length;

                length -= position.Index;
                position.Add(length);

                continue;
            }

            if (c is '\n')
            {
                position.NewLine();

                continue;
            }

            if (c is ' ')
            {
                position.Add(1);

                continue;
            }

            if (char.IsDigit(c))
            {
                var length = 0;
                var integerPart = 0;
                while (code.Length > position.Index + length && char.IsDigit(code[position.Index + length]))
                {
                    integerPart = integerPart * 10 + (code[position.Index + length] - '0');
                    length++;
                }

                if (code.Length > position.Index + length + 1 &&
                    code[position.Index + length] == '.' &&
                    char.IsDigit(code[position.Index + length + 1]))
                {
                    length++;

                    var fractionalPart = 0.0;
                    var divisor = 10.0;

                    while (code.Length > position.Index + length && char.IsDigit(code[position.Index + length]))
                    {
                        fractionalPart += (code[position.Index + length] - '0') / divisor;
                        divisor *= 10.0;
                        length++;
                    }

                    var floatNumber = integerPart + fractionalPart;
                    tokens.Add(Token.CreateFloat(position.Build(length), floatNumber));
                }
                else
                {
                    tokens.Add(Token.CreateInteger(position.Build(length), integerPart));
                }

                continue;
            }

            if (char.IsLetter(c))
            {
                var length = 1;
                while (position.Index + length < code.Length &&
                       char.IsLetterOrDigit(code[position.Index + length]))
                {
                    length++;
                }

                var id = code.Substring(position.Index, length);
                var sourceSpan = position.Build(length);

                tokens.Add(id switch
                {
                    "var" => Token.Create(sourceSpan, TokenKind.Var),
                    "if" => Token.Create(sourceSpan, TokenKind.If),
                    "else" => Token.Create(sourceSpan, TokenKind.Else),
                    "endif" => Token.Create(sourceSpan, TokenKind.EndIf),
                    "external" => Token.Create(sourceSpan, TokenKind.External),
                    "return" => Token.Create(sourceSpan, TokenKind.Return),
                    "true" => Token.Create(sourceSpan, TokenKind.True),
                    "false" => Token.Create(sourceSpan, TokenKind.False),
                    "for" => Token.Create(sourceSpan, TokenKind.For),
                    "while" => Token.Create(sourceSpan, TokenKind.While),
                    "break" => Token.Create(sourceSpan, TokenKind.Break),
                    "continue" => Token.Create(sourceSpan, TokenKind.Continue),
                    "public" => Token.Create(sourceSpan, TokenKind.Public),
                    "internal" => Token.Create(sourceSpan, TokenKind.Internal),
                    "private" => Token.Create(sourceSpan, TokenKind.Private),
                    "type" => Token.Create(sourceSpan, TokenKind.Type),
                    "constructor" => Token.Create(sourceSpan, TokenKind.Constructor),
                    "new" => Token.Create(sourceSpan, TokenKind.New),
                    "null" => Token.Create(sourceSpan, TokenKind.Null),
                    "get" => Token.Create(sourceSpan, TokenKind.Get),
                    "set" => Token.Create(sourceSpan, TokenKind.Set),
                    "static" => Token.Create(sourceSpan, TokenKind.Static),
                    "is" => Token.Create(sourceSpan, TokenKind.Is),

                    _ => Token.CreateId(sourceSpan, id),
                });

                continue;
            }

            if (c is '\'')
            {
                // TODO: handle escape sequences
                var start = position.Current;
                position.Add(1);

                var hasEndQuote = true;
                var endIndex = code.IndexOf('\'', position.Index);
                if (endIndex == -1)
                {
                    hasEndQuote = false;
                    endIndex = code.Length - 1;
                }

                while (position.Index <= endIndex)
                {
                    var currentChar = code[position.Index];
                    if (currentChar is '\n')
                        position.NewLine();
                    else
                        position.Add(1);
                }

                var stringLength = position.Index - start.Index - 1;
                if (hasEndQuote)
                    stringLength--;

                var str = code.Substring(start.Index + 1, stringLength);
                var token = Token.CreateChar(start.ToSpan(position.Current), str);
                tokens.Add(token);

                if (!hasEndQuote)
                    diagnostics.MissingEndQuoteForStringLiteral(token.SourceSpan.End.ToSpan());

                // TODO: if (unescape(str).Length > 1) report error
                continue;
            }

            if (c is '"')
            {
                // TODO: handle escape sequences
                var start = position.Current;
                position.Add(1);

                var hasEndQuote = true;
                var endIndex = code.IndexOf('"', position.Index);
                if (endIndex == -1)
                {
                    hasEndQuote = false;
                    endIndex = code.Length - 1;
                }

                while (position.Index <= endIndex)
                {
                    var currentChar = code[position.Index];
                    if (currentChar is '\n')
                        position.NewLine();
                    else
                        position.Add(1);
                }

                var stringLength = position.Index - start.Index - 1;
                if (hasEndQuote)
                    stringLength--;

                var str = code.Substring(start.Index + 1, stringLength);
                var token = Token.CreateString(start.ToSpan(position.Current), str);
                tokens.Add(token);

                if (!hasEndQuote)
                    diagnostics.MissingEndQuoteForStringLiteral(token.SourceSpan.End.ToSpan());

                continue;
            }

            var (tokenKind, size) = (c, next) switch
            {
                ('(', _) => (TokenKind.OpenParenthesis, 1),
                (')', _) => (TokenKind.CloseParenthesis, 1),
                ('{', _) => (TokenKind.OpenBrace, 1),
                ('}', _) => (TokenKind.CloseBrace, 1),
                ('[', _) => (TokenKind.OpenBracket, 1),
                (']', _) => (TokenKind.CloseBracket, 1),

                (':', _) => (TokenKind.Colon, 1),
                (';', _) => (TokenKind.SemiColon, 1),
                (',', _) => (TokenKind.Comma, 1),

                ('+', '=') => (TokenKind.PlusEqual, 2),
                ('+', _) => (TokenKind.Plus, 1),
                ('-', '=') => (TokenKind.MinusEqual, 2),
                ('-', _) => (TokenKind.Minus, 1),
                ('*', '=') => (TokenKind.AsteriskEqual, 2),
                ('*', _) => (TokenKind.Asterisk, 1),
                ('/', '=') => (TokenKind.SlashEqual, 2),
                ('/', _) => (TokenKind.Slash, 1),

                ('=', '>') => (TokenKind.EqualGreater, 2),

                ('=', '=') => (TokenKind.EqualEqual, 2),
                ('=', _) => (TokenKind.Equal, 1),
                ('!', '=') => (TokenKind.ExclamationEqual, 2),
                ('>', '=') => (TokenKind.GreaterEqual, 2),
                ('<', '=') => (TokenKind.LessEqual, 2),
                ('<', _) => (TokenKind.Less, 1),
                ('>', _) => (TokenKind.Greater, 1),
                ('!', _) => (TokenKind.Exclamation, 1),

                ('&', '&') => (TokenKind.AmpersandAmpersand, 2),
                ('|', '|') => (TokenKind.PipePipe, 2),
                ('&', '=') => (TokenKind.AmpersandEqual, 2),
                ('&', _) => (TokenKind.Ampersand, 1),
                ('|', '=') => (TokenKind.PipeEqual, 2),
                ('|', _) => (TokenKind.Pipe, 1),
                ('^', '=') => (TokenKind.CaretEqual, 2),
                ('^', _) => (TokenKind.Caret, 1),
                ('~', _) => (TokenKind.Tilde, 1),
                ('%', '=') => (TokenKind.PercentEqual, 2),
                ('%', _) => (TokenKind.Percent, 1),

                ('.', _) => (TokenKind.Dot, 1),
                ('#', _) => (TokenKind.Hash, 1),

                _ => (TokenKind.Unsupported, 1),
            };

            var span = position.Build(size);
            if (tokenKind == TokenKind.Unsupported)
                diagnostics.UnsupportedCharacter(span, c);
            else
                tokens.Add(Token.Create(span, tokenKind));
        }

        tokens.Add(Token.CreateEof(position.Build()));

        return tokens;
    }
}