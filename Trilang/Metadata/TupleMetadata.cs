using Trilang.Metadata.Aggregate;

namespace Trilang.Metadata;

public class TupleMetadata : IAnonymousTypeMetadata
{
    private readonly List<ITypeMetadata> types;
    private readonly List<FieldMetadata> fields;
    private readonly List<PropertyMetadata> properties;
    private readonly List<MethodMetadata> methods;
    private bool isFrozen;

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
    {
        EnsureNotFrozen();
        types.Add(type);
    }

    public AggregateMetadata GetMembers(string name)
    {
        var member = GetProperty(name) ??
                     GetMethod(name) ??
                     GetField(name) as IMetadata;

        return member is null
            ? AggregateMetadata.Empty
            : new AggregateMetadata([member]);
    }

    public FieldMetadata? GetField(string name)
        => fields.FirstOrDefault(f => f.Name == name);

    public void AddField(FieldMetadata field)
    {
        EnsureNotFrozen();
        fields.Add(field);
    }

    public PropertyMetadata? GetProperty(string name)
        => properties.FirstOrDefault(f => f.Name == name);

    public void AddProperty(PropertyMetadata property)
    {
        EnsureNotFrozen();
        properties.Add(property);
    }

    public MethodMetadata? GetMethod(string name)
        => methods.FirstOrDefault(f => f.Name == name);

    public void AddMethod(MethodMetadata method)
    {
        EnsureNotFrozen();
        methods.Add(method);
    }

    public void MarkAsInvalid()
    {
        EnsureNotFrozen();
        IsInvalid = true;
    }

    public void Freeze()
    {
        isFrozen = true;

        foreach (var field in fields)
            field.Freeze();

        foreach (var property in properties)
            property.Freeze();

        foreach (var method in methods)
            method.Freeze();
    }

    private void EnsureNotFrozen()
    {
        if (isFrozen)
            throw new InvalidOperationException("Cannot modify frozen metadata.");
    }

    public bool IsInvalid
    {
        get;
        private set
        {
            EnsureNotFrozen();
            field = value;
        }
    }

    public SourceLocation? Definition { get; }

    public bool IsValueType
        => true;

    public TypeLayout? Layout
    {
        get;
        set
        {
            EnsureNotFrozen();
            field = value;
        }
    }

    public INamespaceMetadata? Namespace
    {
        get;
        set
        {
            EnsureNotFrozen();
            field = value;
        }
    }

    public IReadOnlyList<ITypeMetadata> Types
        => types;

    public IReadOnlyList<FieldMetadata> Fields
        => fields;

    public IReadOnlyList<PropertyMetadata> Properties
        => properties;

    public IReadOnlyList<MethodMetadata> Methods
        => methods;
}