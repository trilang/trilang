using Trilang.Metadata;

namespace Trilang.Semantics.TypeMatchers.AggregateResults;

public sealed class SuccessfulMatch<T> : IResolutionResult<T> where T : IMetadata
{
    public SuccessfulMatch(T member)
        => Member = member;

    public T Member { get; }
}