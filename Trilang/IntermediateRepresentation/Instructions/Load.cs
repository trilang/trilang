namespace Trilang.IntermediateRepresentation.Instructions;

public record Load(Register Destination, Register Source) : IInstruction
{
    public override string ToString()
        => $"ld\t{Destination}, {Source}";
}