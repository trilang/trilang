namespace Trilang.Metadata.Aggregate;

public sealed class SuccessfulMatch<T> : IResolutionResult<T> where T : IMetadata
{
    public SuccessfulMatch(T member)
        => Member = member;

    public T Member { get; }
}