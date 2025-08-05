namespace Trilang.IntermediateRepresentation.Instructions;

public record Phi(Register Result, IReadOnlyCollection<Register> Sources) : IInstruction
{
    public virtual bool Equals(Phi? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Result.Equals(other.Result) &&
               Sources.SequenceEqual(other.Sources);
    }

    public override int GetHashCode()
        => HashCode.Combine(Result, Sources);

    public override string ToString()
        => $"phi\t{Result}, {string.Join(", ", Sources)}";
}