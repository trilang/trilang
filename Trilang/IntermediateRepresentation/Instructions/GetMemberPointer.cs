using Trilang.Metadata;

namespace Trilang.IntermediateRepresentation.Instructions;

public record GetMemberPointer(Register Result, Register? Source, IMetadata Member) : IInstruction
{
    public override string ToString()
        => $"get\t{Result}, {Source}, {Member}";
}