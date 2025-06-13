namespace Trilang.IntermediateRepresentation;

public record ArrayElement(Register Result, Register Array, Register Index) : IInstruction;