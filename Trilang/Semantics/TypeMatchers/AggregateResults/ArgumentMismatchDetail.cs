using Trilang.Metadata;

namespace Trilang.Semantics.TypeMatchers.AggregateResults;

public readonly record struct ArgumentMismatchDetail(
    int Position,
    ITypeMetadata Expected,
    ITypeMetadata Actual);