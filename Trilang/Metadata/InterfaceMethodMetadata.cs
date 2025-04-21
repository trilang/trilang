namespace Trilang.Metadata;

public class InterfaceMethodMetadata : IMetadata, IEquatable<InterfaceMethodMetadata>
{
    public InterfaceMethodMetadata(InterfaceMetadata declaringType, string name, FunctionTypeMetadata typeMetadata)
    {
        DeclaringType = declaringType;
        Name = name;
        TypeMetadata = typeMetadata;
    }

    public static bool operator ==(InterfaceMethodMetadata? left, InterfaceMethodMetadata? right)
        => Equals(left, right);

    public static bool operator !=(InterfaceMethodMetadata? left, InterfaceMethodMetadata? right)
        => !Equals(left, right);

    public bool Equals(InterfaceMethodMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return DeclaringType.Equals(other.DeclaringType) &&
               Name == other.Name &&
               TypeMetadata.Equals(other.TypeMetadata);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((InterfaceMethodMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(DeclaringType, Name, TypeMetadata);

    public override string ToString()
        => $"{Name}: {TypeMetadata}";

    public InterfaceMetadata DeclaringType { get; }

    public string Name { get; }

    public FunctionTypeMetadata TypeMetadata { get; }
}