namespace Trilang.IntermediateRepresentation;

public record BranchInstruction(
    Register Condition,
    string ThenBlock,
    string? ElseBlock) : IInstruction;