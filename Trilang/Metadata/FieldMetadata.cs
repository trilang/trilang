namespace Trilang.Metadata;

public class FieldMetadata : IMetadata, IEquatable<FieldMetadata>
{
    public FieldMetadata(
        ITypeMetadata declaringType,
        string name,
        ITypeMetadata type)
    {
        DeclaringType = declaringType;
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

        if (IsInvalid || other.IsInvalid)
            return false;

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

        return Equals((FieldMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name, Type);

    public override string ToString()
        => $"{Name}: {Type}";

    public SourceLocation? Definition => null;

    public bool IsInvalid => false;

    public ITypeMetadata DeclaringType { get; }

    public string Name { get; }

    public ITypeMetadata Type { get; }
}