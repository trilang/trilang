namespace Trilang.Metadata;

public class TupleMetadata : ITypeMetadata, IEquatable<TupleMetadata>
{
    // TODO: combine to a single collection?
    private readonly List<ITypeMetadata> types;
    private readonly List<FieldMetadata> fields;

    public TupleMetadata() : this([])
    {
    }

    public TupleMetadata(IEnumerable<ITypeMetadata> types)
    {
        this.types = [];
        this.fields = [];

        foreach (var type in types)
            AddType(type);
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
    {
        var name = types.Count.ToString();

        types.Add(type);
        fields.Add(new FieldMetadata(this, name, type)); // TODO: use property?
    }

    public IMetadata? GetMember(string name)
        => fields.FirstOrDefault(f => f.Name == name);

    public IReadOnlyList<ITypeMetadata> Types
        => types;

    public IReadOnlyList<FieldMetadata> Fields
        => fields;

    public bool IsValueType
        => true;

    public TypeLayout? Layout { get; set; }
}