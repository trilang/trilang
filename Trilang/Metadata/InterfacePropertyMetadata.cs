namespace Trilang.Metadata;

// TODO: interface getters/setters
public class InterfacePropertyMetadata : IMetadata, IEquatable<InterfacePropertyMetadata>
{
    public InterfacePropertyMetadata(InterfaceMetadata declaringType, string name, ITypeMetadata type)
    {
        DeclaringType = declaringType;
        Name = name;
        Type = type;
    }

    public static bool operator ==(InterfacePropertyMetadata? left, InterfacePropertyMetadata? right)
        => Equals(left, right);

    public static bool operator !=(InterfacePropertyMetadata? left, InterfacePropertyMetadata? right)
        => !Equals(left, right);

    public bool Equals(InterfacePropertyMetadata? other)
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

        return Equals((InterfacePropertyMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(DeclaringType, Name, Type);

    public override string ToString()
        => $"{Name}: {Type}";

    public InterfaceMetadata DeclaringType { get; }

    public string Name { get; }

    public ITypeMetadata Type { get; }
}