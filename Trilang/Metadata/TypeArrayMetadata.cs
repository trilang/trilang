namespace Trilang.Metadata;

public class TypeArrayMetadata : ITypeMetadata, IEquatable<TypeArrayMetadata>
{
    public TypeArrayMetadata() : this(null)
    {
    }

    public TypeArrayMetadata(ITypeMetadata? itemMetadata)
        => ItemMetadata = itemMetadata;

    public static bool operator ==(TypeArrayMetadata? left, TypeArrayMetadata? right)
        => Equals(left, right);

    public static bool operator !=(TypeArrayMetadata? left, TypeArrayMetadata? right)
        => !Equals(left, right);

    public bool Equals(TypeArrayMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Equals(ItemMetadata, other.ItemMetadata);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((TypeArrayMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(ItemMetadata);

    public override string ToString()
        => $"{ItemMetadata}[]";

    public ITypeMetadata? ItemMetadata { get; set; }

    public bool IsValueType
        => false;
}