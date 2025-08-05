namespace Trilang.IntermediateRepresentation.Instructions;

public record UnaryOperation(
    Register Result,
    UnaryInstructionKind Kind,
    Register Operand) : IInstruction
{
    public override string ToString()
    {
        var op = Kind.ToString().ToLower();

        return $"{op}\t{Result}, {Operand}";
    }
}