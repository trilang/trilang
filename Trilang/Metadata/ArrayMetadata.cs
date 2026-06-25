namespace Trilang.Metadata;

public class ArrayMetadata : IAnonymousTypeMetadata
{
    private readonly List<FieldMetadata> fields;
    private readonly List<PropertyMetadata> properties;
    private readonly List<MethodMetadata> methods;
    private bool isFrozen;

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
    {
        EnsureNotFrozen();
        fields.Add(field);
    }

    public AggregateMetadata GetFields(string name)
        => new AggregateMetadata(fields.Where(f => f.Name == name));

    public void AddProperty(PropertyMetadata property)
    {
        EnsureNotFrozen();
        properties.Add(property);
    }

    public AggregateMetadata GetProperties(string name)
        => new AggregateMetadata(properties.Where(f => f.Name == name));

    public void AddMethod(MethodMetadata method)
    {
        EnsureNotFrozen();
        methods.Add(method);
    }

    public AggregateMetadata GetMethods(string name)
        => new AggregateMetadata(methods.Where(m => m.Name == name));

    public AggregateMetadata GetMembers(string name)
        => GetProperties(name)
            .Combine(GetMethods(name))
            .Combine(GetFields(name));

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
        => false;

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

    public IReadOnlyList<FieldMetadata> Fields
        => fields;

    public IReadOnlyList<PropertyMetadata> Properties
        => properties;

    public IReadOnlyList<MethodMetadata> Methods
        => methods;

    public ITypeMetadata? ItemMetadata { get; }
}