using Trilang.Metadata;

namespace Trilang.Semantics.TypeMatchers;

public sealed class BinaryImplicitConversion : ITypeMatchResult, IEquatable<BinaryImplicitConversion>
{
    public BinaryImplicitConversion(ITypeMetadata? left, ITypeMetadata? right)
    {
        Left = left;
        Right = right;

        if (left is null && right is null)
            throw new ArgumentNullException(null, "Both types are null");
    }

    public static bool operator ==(BinaryImplicitConversion? left, BinaryImplicitConversion? right)
        => Equals(left, right);

    public static bool operator !=(BinaryImplicitConversion? left, BinaryImplicitConversion? right)
        => !Equals(left, right);

    public bool Equals(BinaryImplicitConversion? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Equals(Left, other.Left) &&
               Equals(Right, other.Right);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((BinaryImplicitConversion)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Left, Right);

    public ITypeMetadata? Left { get; }

    public ITypeMetadata? Right { get; }
}