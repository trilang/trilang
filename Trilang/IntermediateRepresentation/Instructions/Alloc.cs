namespace Trilang.IntermediateRepresentation.Instructions;

public record Alloc(Register Result, int Size) : IInstruction
{
    public override string ToString()
        => $"alloc\t{Result}, {Size}";
}