using System.Text;
using Trilang.Metadata.Aggregate;

namespace Trilang.Metadata;

public class TypeMetadata : IGenericMetadata, INamedMetadata
{
    public static readonly TypeMetadata InvalidType = Invalid("<>_invalid_type");

    private readonly List<ITypeMetadata> genericArguments;
    private readonly HashSet<InterfaceMetadata> interfaces;
    private readonly List<FieldMetadata> fields;
    private readonly List<PropertyMetadata> properties;
    private readonly List<ConstructorMetadata> constructors;
    private readonly List<MethodMetadata> methods;
    private bool isFrozen;

    public TypeMetadata(
        SourceLocation? definition,
        string name,
        IEnumerable<ITypeMetadata> genericArguments,
        IEnumerable<InterfaceMetadata> interfaces,
        IEnumerable<FieldMetadata> fields,
        IEnumerable<PropertyMetadata> properties,
        IEnumerable<ConstructorMetadata> constructors,
        IEnumerable<MethodMetadata> methods,
        bool isValueType,
        bool isCompilerGenerated)
    {
        Definition = definition;
        Name = name;
        this.genericArguments = [.. genericArguments];
        this.interfaces = [.. interfaces];
        this.fields = [.. fields];
        this.properties = [.. properties];
        this.constructors = [.. constructors];
        this.methods = [.. methods];
        IsValueType = isValueType;
        IsCompilerGenerated = isCompilerGenerated;
    }

    public TypeMetadata(SourceLocation? definition, string name)
        : this(definition, name, [], [], [], [], [], [], false, false)
    {
    }

    public static TypeMetadata Invalid(string name)
        => new TypeMetadata(null, name, [], [], [], [], [], [], false, true) { IsInvalid = true };

    public override string ToString()
    {
        if (!IsGeneric)
            return Name;

        var sb = new StringBuilder();

        if (IsCompilerGenerated)
            sb.Append("<>_");

        sb.Append(Name);
        sb.Append('<');

        for (var i = 0; i < genericArguments.Count; i++)
        {
            var genericArgument = genericArguments[i];
            if (genericArgument is not TypeArgumentMetadata)
                sb.Append(genericArgument);

            if (i < genericArguments.Count - 1)
                sb.Append(',');
        }

        sb.Append('>');

        return sb.ToString();
    }

    public void AddGenericArgument(ITypeMetadata typeArgumentMetadata)
    {
        EnsureNotFrozen();
        genericArguments.Add(typeArgumentMetadata);
    }

    public void AddInterface(InterfaceMetadata interfaceMetadata)
    {
        EnsureNotFrozen();
        interfaces.Add(interfaceMetadata);
    }

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

    public void AddConstructor(ConstructorMetadata constructor)
    {
        EnsureNotFrozen();
        constructors.Add(constructor);
    }

    public AggregateMetadata GetConstructors()
        => new AggregateMetadata(constructors);

    public void AddMethod(MethodMetadata method)
    {
        EnsureNotFrozen();
        methods.Add(method);
    }

    public AggregateMetadata GetMethods(string name)
        => new AggregateMetadata(methods.Where(m => m.Name == name));

    public AggregateMetadata GetMembers(string name)
    {
        var members = GetProperties(name)
            .Combine(GetMethods(name))
            .Combine(GetFields(name));

        if (name == ConstructorMetadata.Name)
            members = members.Combine(GetConstructors());

        return members;
    }

    public void MarkAsInvalid()
    {
        EnsureNotFrozen();
        IsInvalid = true;
    }

    public void Freeze()
    {
        isFrozen = true;

        foreach (var genericArgument in genericArguments)
            genericArgument.Freeze();

        foreach (var field in fields)
            field.Freeze();

        foreach (var property in properties)
            property.Freeze();

        foreach (var constructor in constructors)
            constructor.Freeze();

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

    public bool IsValueType { get; }

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

    public string Name { get; }

    public bool IsCompilerGenerated { get; }

    public IReadOnlyList<ITypeMetadata> GenericArguments => genericArguments;

    public IReadOnlyCollection<InterfaceMetadata> Interfaces => interfaces;

    public IReadOnlyList<PropertyMetadata> Properties => properties;

    public IReadOnlyList<FieldMetadata> Fields => fields;

    public IReadOnlyList<ConstructorMetadata> Constructors => constructors;

    public IReadOnlyList<MethodMetadata> Methods => methods;

    public bool IsGeneric => genericArguments.Count > 0;
}