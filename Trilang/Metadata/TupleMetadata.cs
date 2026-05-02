namespace Trilang.Metadata;

public class TupleMetadata : IAnonymousTypeMetadata
{
    private readonly List<ITypeMetadata> types;
    private readonly List<FieldMetadata> fields;
    private readonly List<PropertyMetadata> properties;
    private readonly List<MethodMetadata> methods;

    public TupleMetadata(SourceLocation? definition)
    {
        Definition = definition;

        types = [];
        fields = [];
        properties = [];
        methods = [];
    }

    public override string ToString()
        => $"({string.Join(", ", types)})";

    public void AddType(ITypeMetadata type)
        => types.Add(type);

    public IMetadata? GetMember(string name)
        => GetProperty(name) ??
           GetMethod(name) ??
           GetField(name) as IMetadata;

    public FieldMetadata? GetField(string name)
        => fields.FirstOrDefault(f => f.Name == name);

    public void AddField(FieldMetadata field)
        => fields.Add(field);

    public PropertyMetadata? GetProperty(string name)
        => properties.FirstOrDefault(f => f.Name == name);

    public void AddProperty(PropertyMetadata property)
        => properties.Add(property);

    public MethodMetadata? GetMethod(string name)
        => methods.FirstOrDefault(f => f.Name == name);

    public void AddMethod(MethodMetadata method)
        => methods.Add(method);

    public void MarkAsInvalid()
        => IsInvalid = true;

    public bool IsInvalid { get; private set; }

    public SourceLocation? Definition { get; }

    public bool IsValueType
        => true;

    public TypeLayout? Layout { get; set; }

    public INamespaceMetadata? Namespace { get; set; }

    public IReadOnlyList<ITypeMetadata> Types
        => types;

    public IReadOnlyList<FieldMetadata> Fields
        => fields;

    public IReadOnlyList<PropertyMetadata> Properties
        => properties;

    public IReadOnlyList<MethodMetadata> Methods
        => methods;
}