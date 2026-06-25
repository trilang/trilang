using Trilang.Metadata;

namespace Trilang.Semantics.TypeMatchers.AggregateResults;

public sealed class NotFunction : IResolutionResult<IFunctionMetadata>
{
    public NotFunction(IMetadata candidate)
        => Candidate = candidate;

    public IFunctionMetadata Member
        => FunctionMetadata.Invalid;

    public IMetadata Candidate { get; }
}