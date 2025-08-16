namespace Trilang.IntermediateRepresentation.Instructions;

public record ArrayAlloc(Register Result, int TypeSize, int ElementSize, Register Count) : IInstruction
{
    public override string ToString()
        => $"alloc\t{Result}, {TypeSize}, {ElementSize}, {Count}";
}