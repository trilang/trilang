namespace Trilang.IntermediateRepresentation.Instructions;

public record LoadInstruction(Register Result, object? Value) : IInstruction
{
    public override string ToString()
        => $"ld {Result}, {Value}";
}