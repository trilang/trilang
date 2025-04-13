namespace Trilang.Metadata;

public class FieldMetadata : IMetadata, IEquatable<FieldMetadata>
{
    public FieldMetadata(AccessModifierMetadata accessModifier, string name, ITypeMetadata type)
    {
        AccessModifier = accessModifier;
        Name = name;
        Type = type;
    }

    public static bool operator ==(FieldMetadata? left, FieldMetadata? right)
        => Equals(left, right);

    public static bool operator !=(FieldMetadata? left, FieldMetadata? right)
        => !Equals(left, right);

    public bool Equals(FieldMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return AccessModifier == other.AccessModifier &&
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

        return Equals((FieldMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name, Type);

    public AccessModifierMetadata AccessModifier { get; }

    public string Name { get; }

    public ITypeMetadata Type { get; }
}