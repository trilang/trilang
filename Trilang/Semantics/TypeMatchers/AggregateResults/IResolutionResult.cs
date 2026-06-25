using Trilang.Metadata;

namespace Trilang.Semantics.TypeMatchers.AggregateResults;

public interface IResolutionResult<out T> where T : IMetadata
{
    T Member { get; }
}