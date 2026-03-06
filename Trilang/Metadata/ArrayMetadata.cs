namespace Trilang.Metadata;

public class ArrayMetadata : IAnonymousTypeMetadata, IEquatable<ArrayMetadata>
{
    private readonly List<FieldMetadata> fields;
    private readonly List<PropertyMetadata> properties;
    private readonly List<MethodMetadata> methods;

    public ArrayMetadata(SourceLocation? definition, ITypeMetadata? itemMetadata)
    {
        fields = [];
        properties = [];
        methods = [];

        Definition = definition;
        ItemMetadata = itemMetadata;
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

        return Equals(ItemMetadata, other.ItemMetadata) &&
               Equals(Namespace, other.Namespace);
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

    public void AddField(FieldMetadata field)
        => fields.Add(field);

    public AggregateMetadata GetFields(string name)
        => new AggregateMetadata(fields.Where(f => f.Name == name));

    public void AddProperty(PropertyMetadata property)
        => properties.Add(property);

    public AggregateMetadata GetProperties(string name)
        => new AggregateMetadata(properties.Where(f => f.Name == name));

    public void AddMethod(MethodMetadata method)
        => methods.Add(method);

    public AggregateMetadata GetMethods(string name)
        => new AggregateMetadata(methods.Where(m => m.Name == name));

    public IMetadata? GetMember(string name)
    {
        var aggregate = GetProperties(name)
            .Combine(GetMethods(name))
            .Combine(GetFields(name));

        return aggregate.Members switch
        {
            [] => null,
            [var single] => single,
            _ => aggregate,
        };
    }

    public void MarkAsInvalid()
        => IsInvalid = true;

    public bool IsInvalid { get; private set; }

    public SourceLocation? Definition { get; }

    public bool IsValueType
        => false;

    public TypeLayout? Layout { get; set; }

    public NamespaceMetadata? Namespace { get; set; }

    public IReadOnlyList<FieldMetadata> Fields
        => fields;

    public IReadOnlyList<PropertyMetadata> Properties
        => properties;

    public IReadOnlyList<MethodMetadata> Methods
        => methods;

    public ITypeMetadata? ItemMetadata { get; }
}