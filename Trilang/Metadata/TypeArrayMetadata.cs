namespace Trilang.Metadata;

public class TypeArrayMetadata : ITypeMetadata, IEquatable<TypeArrayMetadata>
{
    private readonly List<FieldMetadata> fields;
    private readonly List<PropertyMetadata> properties;
    private readonly List<MethodMetadata> methods;

    public TypeArrayMetadata() : this(null)
    {
    }

    public TypeArrayMetadata(ITypeMetadata? itemMetadata)
    {
        this.fields = [];
        this.properties = [];
        this.methods = [];

        ItemMetadata = itemMetadata;

        fields.Add(new FieldMetadata(this, "<>_size", TypeMetadata.I64));

        var sizeProperty = new PropertyMetadata(this, "size", TypeMetadata.I64);
        properties.Add(sizeProperty);
        methods.Add(sizeProperty.Getter);
        methods.Add(sizeProperty.Setter);
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
        => GetProperty(name) ??
           GetMethod(name) ??
           GetField(name) as IMetadata;

    public FieldMetadata? GetField(string name)
        => fields.FirstOrDefault(f => f.Name == name);

    public PropertyMetadata? GetProperty(string name)
        => properties.FirstOrDefault(f => f.Name == name);

    public MethodMetadata? GetMethod(string name)
        => methods.FirstOrDefault(f => f.Name == name);

    public IReadOnlyList<FieldMetadata> Fields
        => fields;

    public IReadOnlyList<PropertyMetadata> Properties
        => properties;

    public IReadOnlyList<MethodMetadata> Methods
        => methods;

    public ITypeMetadata? ItemMetadata { get; set; }

    public bool IsValueType
        => false;

    public TypeLayout? Layout { get; set; }
}