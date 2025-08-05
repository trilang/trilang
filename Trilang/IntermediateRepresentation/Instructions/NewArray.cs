using Trilang.Metadata;

namespace Trilang.IntermediateRepresentation.Instructions;

public record NewArray(Register Result, TypeArrayMetadata Type, Register Size) : IInstruction
{
    public override string ToString()
        => $"new\t{Result}, {Type}, {Size}";
}