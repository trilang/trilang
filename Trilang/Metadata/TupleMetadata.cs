namespace Trilang.Metadata;

public class TupleMetadata : ITypeMetadata, IEquatable<TupleMetadata>
{
    private readonly List<ITypeMetadata> types;

    public TupleMetadata(string name) : this(name, [])
    {
    }

    public TupleMetadata(string name, IEnumerable<ITypeMetadata> types)
    {
        Name = name;
        this.types = new List<ITypeMetadata>(types);
    }

    public static bool operator ==(TupleMetadata? left, TupleMetadata? right)
        => Equals(left, right);

    public static bool operator !=(TupleMetadata? left, TupleMetadata? right)
        => !Equals(left, right);

    public bool Equals(TupleMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name.Equals(other.Name);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((TupleMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name);

    public override string ToString()
        => Name;

    public void AddType(ITypeMetadata type)
        => types.Add(type);

    public string Name { get; }

    public IReadOnlyList<ITypeMetadata> Types
        => types;
}