namespace Trilang.IntermediateRepresentation.Instructions;

public record Store(Register Destination, Register Source) : IInstruction
{
    public override string ToString()
        => $"st\t{Destination}, {Source}";
}