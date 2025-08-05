namespace Trilang.IntermediateRepresentation.Instructions;

public record Move(Register Result, Register Source) : IInstruction
{
    public override string ToString()
        => $"mov {Result}, {Source}";
}