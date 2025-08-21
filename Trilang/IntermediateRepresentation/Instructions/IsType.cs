using Trilang.Metadata;

namespace Trilang.IntermediateRepresentation.Instructions;

public record IsType(Register Result, Register Source, ITypeMetadata Type) : IInstruction
{
    public override string ToString()
        => $"is\t{Result}, {Source}, {Type}";
}