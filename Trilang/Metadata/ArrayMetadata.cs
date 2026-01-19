namespace Trilang.Metadata;

public class ArrayMetadata : ITypeMetadata, IEquatable<ArrayMetadata>
{
    private readonly List<FieldMetadata> fields;
    private readonly List<PropertyMetadata> properties;
    private readonly List<MethodMetadata> methods;

    public ArrayMetadata(SourceLocation? definition, ITypeMetadata? itemMetadata)
    {
        Definition = definition;
        this.fields = [];
        this.properties = [];
        this.methods = [];

        ItemMetadata = itemMetadata;

        fields.Add(new FieldMetadata(this, "<>_size", TypeMetadata.I64));

        var sizeProperty = new PropertyMetadata(null, this, "size", TypeMetadata.I64, AccessModifierMetadata.Public);
        properties.Add(sizeProperty);
        methods.Add(sizeProperty.Getter!);
    }

    public static ArrayMetadata Invalid()
        => new ArrayMetadata(null, null) { IsInvalid = true };

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

        if (IsInvalid || other.IsInvalid)
            return false;

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

        return Equals((ArrayMetadata)obj);
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

    public bool IsInvalid { get; private set; }

    public SourceLocation? Definition { get; }

    public bool IsValueType
        => false;

    public TypeLayout? Layout { get; set; }

    public IReadOnlyList<FieldMetadata> Fields
        => fields;

    public IReadOnlyList<PropertyMetadata> Properties
        => properties;

    public IReadOnlyList<MethodMetadata> Methods
        => methods;

    public ITypeMetadata? ItemMetadata { get; }
}