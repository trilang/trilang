namespace Trilang.Metadata;

public class InterfaceFieldMetadata : IMetadata, IEquatable<InterfaceFieldMetadata>
{
    public InterfaceFieldMetadata(InterfaceMetadata declaringType, string name, ITypeMetadata type)
    {
        DeclaringType = declaringType;
        Name = name;
        Type = type;
    }

    public static bool operator ==(InterfaceFieldMetadata? left, InterfaceFieldMetadata? right)
        => Equals(left, right);

    public static bool operator !=(InterfaceFieldMetadata? left, InterfaceFieldMetadata? right)
        => !Equals(left, right);

    public bool Equals(InterfaceFieldMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return DeclaringType.Equals(other.DeclaringType) &&
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

        return Equals((InterfaceFieldMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(DeclaringType, Name, Type);

    public override string ToString()
        => $"{Name}: {Type}";

    public InterfaceMetadata DeclaringType { get; }

    public string Name { get; }

    public ITypeMetadata Type { get; }
}