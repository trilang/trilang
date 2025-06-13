using Trilang.Metadata;

namespace Trilang.IntermediateRepresentation;

public record NewArrayInstruction(Register Result, TypeArrayMetadata Type, Register Size) : IInstruction;