namespace Trilang.Metadata.Aggregate;

public readonly record struct ArgumentMismatchDetail(int Position, ITypeMetadata Expected, ITypeMetadata Actual);