using Trilang.Metadata;

namespace Trilang.Semantics.TypeMatchers.AggregateResults;

public readonly record struct ConversionMatchDetails(int Position, ITypeMetadata Target);