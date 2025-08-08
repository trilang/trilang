namespace Trilang.Metadata;

public class TupleMetadata : ITypeMetadata, IEquatable<TupleMetadata>
{
    private readonly List<ITypeMetadata> types;

    public TupleMetadata() : this([])
    {
    }

    public TupleMetadata(IEnumerable<ITypeMetadata> types)
        => this.types = [..types];

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

        return Equals((TupleMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(types);

    public override string ToString()
        => $"({string.Join(", ", types)})";

    public void AddType(ITypeMetadata type)
        => types.Add(type);

    public IMetadata? GetMember(string name)
        => null;

    public IReadOnlyList<ITypeMetadata> Types
        => types;

    public bool IsValueType
        => true;
}