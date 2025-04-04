using System.Diagnostics.CodeAnalysis;
using Trilang.Lexing;
using Trilang.Parsing.Ast;

namespace Trilang.Parsing;

internal sealed class TokenReader
{
    private readonly IReadOnlyList<Token> tokens;
    private int index;

    public TokenReader(IReadOnlyList<Token> tokens)
    {
        this.tokens = tokens;
        index = 0;
    }

    public void Advance()
    {
        if (!HasEnded)
            index++;
    }

    public bool Check(TokenKind kind)
        => Check(kind, out _);

    public bool Check(TokenKind kind, [NotNullWhen(true)] out Token? token)
    {
        token = null;

        var result = Current.Is(kind);
        if (result)
        {
            token = Current;
            Advance();
        }

        return result;
    }

    public TResult Scoped<TResult>(ParserContext context, Func<ParserContext, TResult> action)
        where TResult : ISyntaxNode?
    {
        var savedIndex = index;

        var result = action(context);
        if (result is null)
            index = savedIndex;

        return result;
    }

    public Token Current
        => tokens[index];

    public bool HasEnded
        => index >= tokens.Count || tokens[index].Is(TokenKind.EndOfFile);
}