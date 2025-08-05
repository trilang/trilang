namespace Trilang.IntermediateRepresentation.Instructions;

public record LoadConst(Register Result, object? Value) : IInstruction
{
    public override string ToString()
        => $"ldc {Result}, {Value}";
}