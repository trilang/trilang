namespace Trilang.Metadata.Aggregate;

public sealed class NotGeneric : IResolutionResult<IGenericMetadata>
{
    public NotGeneric(IMetadata candidate)
        => Candidate = candidate;

    public IGenericMetadata Member
        => TypeMetadata.InvalidType;

    public IMetadata Candidate { get; }
}