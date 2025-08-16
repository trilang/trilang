namespace Trilang.IntermediateRepresentation.Instructions;

public record GetElementPointer(Register Result, Register Array, Register Index) : IInstruction
{
    public override string ToString()
        => $"get\t{Result}, {Array}, {Index}";
}