namespace Trilang.Metadata;

public class DiscriminatedUnionType : ITypeMetadata, IEquatable<DiscriminatedUnionType>
{
    private readonly List<ITypeMetadata> types;

    public DiscriminatedUnionType(string name)
    {
        Name = name;
        types = [];
    }

    public static bool operator ==(DiscriminatedUnionType? left, DiscriminatedUnionType? right)
        => Equals(left, right);

    public static bool operator !=(DiscriminatedUnionType? left, DiscriminatedUnionType? right)
        => !Equals(left, right);

    public bool Equals(DiscriminatedUnionType? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name &&
               types.SequenceEqual(other.types);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((DiscriminatedUnionType)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(types);

    public override string ToString()
        => $"{Name} = {string.Join(" | ", types)}";

    public void AddType(ITypeMetadata type)
        => types.Add(type);

    public string Name { get; }

    public IReadOnlyList<ITypeMetadata> Types => types;
}