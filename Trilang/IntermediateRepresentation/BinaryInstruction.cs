namespace Trilang.IntermediateRepresentation;

public record BinaryInstruction(
    Register Result,
    BinaryInstructionKind Kind,
    Register Left,
    Register Right) : IInstruction;