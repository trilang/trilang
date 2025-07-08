namespace Trilang.IntermediateRepresentation.Instructions;

public record LoadParameterInstruction(Register Result, int Index) : IInstruction
{
    public override string ToString()
        => $"ldp {Result}, {Index}";
}