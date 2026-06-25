namespace Trilang.Semantics.TypeMatchers;

public sealed class FailedMatch : ITypeMatchResult
{
    public static FailedMatch Instance { get; } = new FailedMatch();

    private FailedMatch()
    {
    }
}