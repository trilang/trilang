using Trilang.Metadata;

namespace Trilang.Semantics.TypeMatchers.AggregateResults;

public sealed class NotGeneric : IResolutionResult<IGenericMetadata>
{
    public NotGeneric(IMetadata candidate)
        => Candidate = candidate;

    public IGenericMetadata Member
        => TypeMetadata.InvalidType;

    public IMetadata Candidate { get; }
}