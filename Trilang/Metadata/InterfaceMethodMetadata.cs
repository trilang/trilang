namespace Trilang.Metadata;

public class InterfaceMethodMetadata : IMetadata, IEquatable<InterfaceMethodMetadata>
{
    public InterfaceMethodMetadata(
        SourceLocation? definition,
        InterfaceMetadata declaringType,
        string name,
        FunctionTypeMetadata type)
    {
        Definition = definition;
        DeclaringType = declaringType;
        Name = name;
        Type = type;
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

        return Equals((InterfaceMethodMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(DeclaringType, Name, Type);

    public override string ToString()
        => $"{Name}: {Type}";

    public SourceLocation? Definition { get; }

    public InterfaceMetadata DeclaringType { get; }

    public string Name { get; }

    public FunctionTypeMetadata Type { get; }
}