namespace Trilang.IntermediateRepresentation.Instructions;

public record JumpInstruction(string Block) : IInstruction
{
    public override string ToString()
        => $"jmp {Block}";
}