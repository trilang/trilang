namespace Trilang.Metadata;

public class TypePointerMetadata : ITypeMetadata, IEquatable<TypePointerMetadata>
{
    public TypePointerMetadata(ITypeMetadata type)
        => Type = type;

    public static bool operator ==(TypePointerMetadata? left, TypePointerMetadata? right)
        => Equals(left, right);

    public static bool operator !=(TypePointerMetadata? left, TypePointerMetadata? right)
        => !Equals(left, right);

    public bool Equals(TypePointerMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Type.Equals(other.Type);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((TypePointerMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Type);

    public override string ToString()
        => $"{Type}*";

    public IMetadata? GetMember(string name)
        => Type.GetMember(name);

    public bool IsInvalid { get; }

    public SourceLocation? Definition => Type.Definition;

    public bool IsValueType
        => true;

    public TypeLayout? Layout { get; set; }

    public ITypeMetadata Type { get; }
}