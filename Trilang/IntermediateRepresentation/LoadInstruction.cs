namespace Trilang.IntermediateRepresentation;

public record LoadInstruction(Register Result, object? Value) : IInstruction;