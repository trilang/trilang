using Trilang.Metadata;

namespace Trilang.Semantics.TypeMatchers;

public sealed class ImplicitConversion : ITypeMatchResult, IEquatable<ImplicitConversion>
{
    public ImplicitConversion(ITypeMetadata target)
        => Target = target;

    public static bool operator ==(ImplicitConversion? left, ImplicitConversion? right)
        => Equals(left, right);

    public static bool operator !=(ImplicitConversion? left, ImplicitConversion? right)
        => !Equals(left, right);

    public bool Equals(ImplicitConversion? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Target.Equals(other.Target);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((ImplicitConversion)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Target);

    public ITypeMetadata Target { get; }
}