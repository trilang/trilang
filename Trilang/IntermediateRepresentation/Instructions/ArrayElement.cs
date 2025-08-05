namespace Trilang.IntermediateRepresentation.Instructions;

public record ArrayElement(Register Result, Register Array, Register Index) : IInstruction
{
    public override string ToString()
        => $"elem\t{Result}, {Array}, {Index}";
}