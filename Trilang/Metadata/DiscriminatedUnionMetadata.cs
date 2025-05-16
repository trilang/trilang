namespace Trilang.Metadata;

public class DiscriminatedUnionMetadata : ITypeMetadata, IEquatable<DiscriminatedUnionMetadata>
{
    private readonly List<ITypeMetadata> types;

    public DiscriminatedUnionMetadata(string name) : this(name, [])
    {
    }

    public DiscriminatedUnionMetadata(string name, IEnumerable<ITypeMetadata> types)
    {
        Name = name;
        this.types = new List<ITypeMetadata>(types);
    }

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

        return Name == other.Name;
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
        => Name;

    public void AddType(ITypeMetadata type)
        => types.Add(type);

    public string Name { get; }

    public IReadOnlyList<ITypeMetadata> Types => types;
}