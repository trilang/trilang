namespace Trilang.Metadata;

public class PropertyMetadata : IMetadata, IEquatable<PropertyMetadata>
{
    public PropertyMetadata(
        TypeMetadata declaringType,
        AccessModifierMetadata accessModifier,
        string name,
        ITypeMetadata type)
    {
        DeclaringType = declaringType;
        AccessModifier = accessModifier;
        Name = name;
        Type = type;
    }

    public static bool operator ==(PropertyMetadata? left, PropertyMetadata? right)
        => Equals(left, right);

    public static bool operator !=(PropertyMetadata? left, PropertyMetadata? right)
        => !Equals(left, right);

    public bool Equals(PropertyMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return DeclaringType.Equals(other.DeclaringType) &&
               AccessModifier == other.AccessModifier &&
               Name == other.Name &&
               Type.Equals(other.Type);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((PropertyMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(AccessModifier, Name, Type);

    public override string ToString()
        => $"{Name}: {Type}";

    public TypeMetadata DeclaringType { get; }

    public AccessModifierMetadata AccessModifier { get; }

    public string Name { get; }

    public ITypeMetadata Type { get; }
}