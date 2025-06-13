namespace Trilang.IntermediateRepresentation;

public record LoadParameterInstruction(Register Result, int Index) : IInstruction;