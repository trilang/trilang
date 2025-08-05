namespace Trilang.IntermediateRepresentation.Instructions;

public record BinaryOperation(
    Register Result,
    BinaryInstructionKind Kind,
    Register Left,
    Register Right) : IInstruction
{
    public override string ToString()
    {
        var op = Kind.ToString().ToLower();

        return $"{op} {Result}, {Left}, {Right}";
    }
}