namespace Trilang.IntermediateRepresentation.Instructions;

public record LoadParameter(Register Result, int Index) : IInstruction
{
    public override string ToString()
        => $"ldp {Result}, {Index}";
}