namespace Trilang.IntermediateRepresentation.Instructions;

public record Return(Register? Expression) : IInstruction
{
    public override string ToString()
        => Expression is null ? "ret" : $"ret\t{Expression}";
}