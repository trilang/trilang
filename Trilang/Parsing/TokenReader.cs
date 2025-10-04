using System.Diagnostics;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;

namespace Trilang.Parsing;

internal sealed class TokenReader
{
    private readonly ParserDiagnosticCollection diagnostics;
    private readonly IReadOnlyList<Token> tokens;
    private int index;

    public TokenReader(ParserDiagnosticCollection diagnostics, IReadOnlyList<Token> tokens)
    {
        this.diagnostics = diagnostics;
        this.tokens = tokens;
        index = 0;
    }

    public void Advance()
    {
        if (!HasEnded)
            index++;
    }

    public CheckResult Check(TokenKind kind)
    {
        var token = Token;
        var result = token.Is(kind);
        if (result)
            Advance();

        return new CheckResult(result, token);
    }

    public SourceSpan SkipTo(params Span<TokenKind> tokenKinds)
    {
        Debug.Assert(tokenKinds.Length > 0, "There should be at least one token kind to skip to.");

        var start = Token.SourceSpan.Start;

        while (true)
        {
            if (HasEnded)
                break;

            var found = false;
            foreach (var tokenKind in tokenKinds)
            {
                if (Token.Is(tokenKind))
                {
                    found = true;
                    break;
                }
            }

            if (found)
                break;

            Advance();
        }

        return start.ToSpan(Token.SourceSpan.Start);
    }

    public Token Peek()
    {
        if (HasEnded)
            throw new InvalidOperationException("Cannot peek at end of file.");

        if (index + 1 >= tokens.Count)
            throw new InvalidOperationException("Cannot peek beyond end of file.");

        return tokens[index + 1];
    }

    public SourceSpan Expect(TokenKind kind)
    {
        var (result, token) = Check(kind);
        if (!result)
        {
            var position = Token.SourceSpan.Start;
            diagnostics.MissingToken(position, kind);

            return position.ToSpan();
        }

        return token.SourceSpan;
    }

    public TResult Scoped<TResult>(ParserContext context, Func<ParserContext, TResult> action)
    {
        var savedIndex = index;

        var result = action(context);
        if (result is null)
            index = savedIndex;

        return result;
    }

    public Token Token
        => tokens[index];

    public SourceSpan Span
        => Token.SourceSpan;

    public bool HasEnded
        => index >= tokens.Count || tokens[index].Is(TokenKind.EndOfFile);
}