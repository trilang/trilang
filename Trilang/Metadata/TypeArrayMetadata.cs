namespace Trilang.Metadata;

public class TypeArrayMetadata : ITypeMetadata, IEquatable<TypeArrayMetadata>
{
    private readonly List<FieldMetadata> fields;

    public TypeArrayMetadata() : this(null)
    {
    }

    public TypeArrayMetadata(ITypeMetadata? itemMetadata)
    {
        fields = [];
        ItemMetadata = itemMetadata;

        fields.Add(new FieldMetadata(this, "size", TypeMetadata.I64)); // TODO: use property?
    }

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

    public IMetadata? GetMember(string name)
        => Fields.FirstOrDefault(x => x.Name == name);

    public IReadOnlyList<FieldMetadata> Fields
        => fields;

    public ITypeMetadata? ItemMetadata { get; set; }

    public bool IsValueType
        => false;

    public TypeLayout? Layout { get; set; }
}