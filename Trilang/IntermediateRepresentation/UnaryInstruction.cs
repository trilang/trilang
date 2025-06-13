namespace Trilang.IntermediateRepresentation;

public record UnaryInstruction(
    Register Result,
    UnaryInstructionKind Kind,
    Register Operand) : IInstruction;