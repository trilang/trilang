using Trilang.Lexing;

namespace Trilang.Parsing;

internal readonly struct CheckResult
{
    public CheckResult(bool result, Token token)
    {
        Result = result;
        Token = token;
    }

    public static implicit operator bool(CheckResult result)
        => result.Result;

    public static bool operator true(CheckResult result)
        => result.Result;

    public static bool operator false(CheckResult result)
        => !result.Result;

    public void Deconstruct(out bool result, out Token token)
    {
        result = Result;
        token = Token;
    }

    public bool Result { get; }

    public Token Token { get; }
}
