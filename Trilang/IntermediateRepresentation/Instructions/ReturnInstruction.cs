namespace Trilang.IntermediateRepresentation.Instructions;

public record ReturnInstruction(Register? Expression) : IInstruction
{
    public override string ToString()
        => Expression is null ? "ret" : $"ret {Expression}";
}