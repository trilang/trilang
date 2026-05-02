namespace Trilang.Metadata;

public class ArrayMetadata : IAnonymousTypeMetadata
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

    public INamespaceMetadata? Namespace { get; set; }

    public IReadOnlyList<FieldMetadata> Fields
        => fields;

    public IReadOnlyList<PropertyMetadata> Properties
        => properties;

    public IReadOnlyList<MethodMetadata> Methods
        => methods;

    public ITypeMetadata? ItemMetadata { get; }
}