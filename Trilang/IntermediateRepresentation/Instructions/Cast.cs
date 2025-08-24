using Trilang.Metadata;

namespace Trilang.IntermediateRepresentation.Instructions;

public record Cast(Register Result, Register Source, ITypeMetadata Type) : IInstruction
{
    public override string ToString()
        => $"cast\t{Result}, {Source}, {Type}";
}