namespace Trilang.Metadata;

public class PropertyGetterMetadata : IMetadata, IEquatable<PropertyGetterMetadata>
{
    public PropertyGetterMetadata(PropertyMetadata declaringProperty, AccessModifierMetadata accessModifier)
    {
        DeclaringProperty = declaringProperty;
        AccessModifier = accessModifier;
    }

    public static bool operator ==(PropertyGetterMetadata? left, PropertyGetterMetadata? right)
        => Equals(left, right);

    public static bool operator !=(PropertyGetterMetadata? left, PropertyGetterMetadata? right)
        => !Equals(left, right);

    public bool Equals(PropertyGetterMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return DeclaringProperty.Equals(other.DeclaringProperty) &&
               AccessModifier == other.AccessModifier;
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((PropertyGetterMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(DeclaringProperty, (int)AccessModifier);

    public PropertyMetadata DeclaringProperty { get; }

    public AccessModifierMetadata AccessModifier { get; }
}