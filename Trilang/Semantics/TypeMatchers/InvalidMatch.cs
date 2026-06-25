namespace Trilang.Semantics.TypeMatchers;

public sealed class InvalidMatch : ITypeMatchResult
{
    public static InvalidMatch Instance { get; } = new InvalidMatch();

    private InvalidMatch()
    {
    }
}