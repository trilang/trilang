namespace Trilang.Semantics.TypeMatchers;

public sealed class SuccessMatch : ITypeMatchResult
{
    public static SuccessMatch Instance { get; } = new SuccessMatch();

    private SuccessMatch()
    {
    }
}