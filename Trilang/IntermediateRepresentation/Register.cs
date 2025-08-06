using Trilang.Metadata;

namespace Trilang.IntermediateRepresentation;

public readonly record struct Register(int Id, ITypeMetadata Type)
{
    public override string ToString()
        => $"#{Id}: {Type}";
}