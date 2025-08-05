namespace Trilang.IntermediateRepresentation.Instructions;

public record Jump(string Block) : IInstruction
{
    public override string ToString()
        => $"jmp\t{Block}";
}