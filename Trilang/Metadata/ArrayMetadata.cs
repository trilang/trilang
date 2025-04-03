namespace Trilang.Metadata;

public class ArrayMetadata : IMetadata, IEquatable<ArrayMetadata>
{
    public ArrayMetadata(IMetadata itemMetadata)
        => ItemMetadata = itemMetadata;

    public static bool operator ==(ArrayMetadata? left, ArrayMetadata? right)
        => Equals(left, right);

    public static bool operator !=(ArrayMetadata? left, ArrayMetadata? right)
        => !Equals(left, right);

    public bool Equals(ArrayMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return ItemMetadata.Equals(other.ItemMetadata);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((ArrayMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(ItemMetadata);

    public IMetadata ItemMetadata { get; }
}