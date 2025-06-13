namespace Trilang.IntermediateRepresentation;

public record ReturnInstruction(Register? Result) : IInstruction;