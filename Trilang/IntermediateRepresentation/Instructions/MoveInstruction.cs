namespace Trilang.IntermediateRepresentation.Instructions;

public record MoveInstruction(Register Result, Register Source) : IInstruction
{
    public override string ToString()
        => $"mov {Result}, {Source}";
}