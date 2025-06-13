namespace Trilang.IntermediateRepresentation;

public record MoveInstruction(Register Result, Register Source) : IInstruction;