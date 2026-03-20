using Trilang.Lexing;

namespace Trilang.Parsing;

internal readonly struct MatchResult
{
    public MatchResult(bool result, Token token)
    {
        Result = result;
        Token = token;
    }

    public static implicit operator bool(MatchResult result)
        => result.Result;

    public static bool operator true(MatchResult result)
        => result.Result;

    public static bool operator false(MatchResult result)
        => !result.Result;

    public void Deconstruct(out bool result, out Token token)
    {
        result = Result;
        token = Token;
    }

    public bool Result { get; }

    public Token Token { get; }
}
