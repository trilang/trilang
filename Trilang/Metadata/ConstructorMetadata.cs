namespace Trilang.Metadata;

public class ConstructorMetadata : IMetadata, IEquatable<ConstructorMetadata>
{
    public ConstructorMetadata(
        TypeMetadata declaringType,
        AccessModifierMetadata accessModifier,
        IReadOnlyCollection<ITypeMetadata> parameterTypes)
    {
        DeclaringType = declaringType;
        AccessModifier = accessModifier;
        ParameterTypes = parameterTypes;
    }

    public static bool operator ==(ConstructorMetadata? left, ConstructorMetadata? right)
        => Equals(left, right);

    public static bool operator !=(ConstructorMetadata? left, ConstructorMetadata? right)
        => !Equals(left, right);

    public bool Equals(ConstructorMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return DeclaringType.Equals(other.DeclaringType) &&
               AccessModifier == other.AccessModifier &&
               ParameterTypes.SequenceEqual(other.ParameterTypes);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((ConstructorMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(AccessModifier, ParameterTypes);

    public TypeMetadata DeclaringType { get; }

    public AccessModifierMetadata AccessModifier { get; }

    public IReadOnlyCollection<ITypeMetadata> ParameterTypes { get; }
}