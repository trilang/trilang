namespace Trilang.Metadata;

public class DiscriminatedUnionMetadata : ITypeMetadata, IEquatable<DiscriminatedUnionMetadata>
{
    private readonly List<ITypeMetadata> types;

    public DiscriminatedUnionMetadata() : this([])
    {
    }

    public DiscriminatedUnionMetadata(IEnumerable<ITypeMetadata> types)
        => this.types = [..types];

    public static bool operator ==(DiscriminatedUnionMetadata? left, DiscriminatedUnionMetadata? right)
        => Equals(left, right);

    public static bool operator !=(DiscriminatedUnionMetadata? left, DiscriminatedUnionMetadata? right)
        => !Equals(left, right);

    public bool Equals(DiscriminatedUnionMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return types.SequenceEqual(other.types);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((DiscriminatedUnionMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(types);

    public override string ToString()
        => string.Join(" | ", types);

    public void AddType(ITypeMetadata type)
        => types.Add(type);

    public bool HasType(TypeMetadata type)
        => types.Contains(type);

    public IReadOnlyList<ITypeMetadata> Types => types;
}