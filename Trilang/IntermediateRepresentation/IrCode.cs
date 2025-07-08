namespace Trilang.IntermediateRepresentation;

public record IrCode(Block Entry)
{
    internal int RegisterCounter { get; init; }
}