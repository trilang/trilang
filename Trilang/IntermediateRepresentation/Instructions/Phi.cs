namespace Trilang.IntermediateRepresentation.Instructions;

public class Phi : IEquatable<Phi>, IInstruction
{
    public Phi(Register result, IReadOnlyCollection<Register> sources)
    {
        Result = result;
        Sources = sources;
    }

    public static bool operator ==(Phi? left, Phi? right)
        => Equals(left, right);

    public static bool operator !=(Phi? left, Phi? right)
        => !Equals(left, right);

    public bool Equals(Phi? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Result.Equals(other.Result) &&
               Sources.SequenceEqual(other.Sources);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((Phi)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Result, Sources);

    public override string ToString()
        => $"phi\t{Result}, {string.Join(", ", Sources)}";

    public Register Result { get; }

    public IReadOnlyCollection<Register> Sources { get; }
}