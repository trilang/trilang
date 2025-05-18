namespace Trilang.Metadata;

public class PropertyMetadata : IMetadata, IEquatable<PropertyMetadata>
{
    public PropertyMetadata(
        TypeMetadata declaringType,
        string name,
        ITypeMetadata type,
        AccessModifierMetadata getterModifier = AccessModifierMetadata.Public,
        AccessModifierMetadata setterModifier = AccessModifierMetadata.Private)
    {
        DeclaringType = declaringType;
        Name = name;
        Type = type;
        GetterModifier = getterModifier;
        SetterModifier = setterModifier;
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
               Name == other.Name &&
               Type.Equals(other.Type) &&
               GetterModifier == other.GetterModifier &&
               SetterModifier == other.SetterModifier;
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
        => HashCode.Combine(Name, Type);

    public override string ToString()
        => $"{Name}: {Type}";

    public TypeMetadata DeclaringType { get; }

    public string Name { get; }

    public AccessModifierMetadata GetterModifier { get; }

    public AccessModifierMetadata SetterModifier { get; }

    public ITypeMetadata Type { get; }
}