namespace Trilang.IntermediateRepresentation;

public readonly record struct Register(int Id)
{
    public override string ToString()
        => $"#{Id}";
}