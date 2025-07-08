using Trilang.Metadata;

namespace Trilang.IntermediateRepresentation.Instructions;

public record NewArrayInstruction(Register Result, TypeArrayMetadata Type, Register Size) : IInstruction
{
    public override string ToString()
        => $"newarr {Result}, {Type}, {Size}";
}