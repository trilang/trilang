using System.Text;

namespace Trilang.Metadata;

// TODO: immutable metadata
public class TypeMetadata : IGenericMetadata, INamedMetadata
{
    public static readonly TypeMetadata InvalidType = Invalid("<>_invalid_type");

    private readonly List<ITypeMetadata> genericArguments;
    private readonly HashSet<InterfaceMetadata> interfaces;
    private readonly List<FieldMetadata> fields;
    private readonly List<PropertyMetadata> properties;
    private readonly List<ConstructorMetadata> constructors;
    private readonly List<MethodMetadata> methods;

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

    public TypeMetadata(
        SourceLocation? definition,
        string name,
        IEnumerable<ITypeMetadata> genericArguments,
        IEnumerable<InterfaceMetadata> interfaces,
        IEnumerable<FieldMetadata> fields,
        IEnumerable<PropertyMetadata> properties,
        IEnumerable<ConstructorMetadata> constructors,
        IEnumerable<MethodMetadata> methods) : this(definition, name, genericArguments, interfaces, fields, properties, constructors, methods, false, false)
    {
    }

    public static TypeMetadata Invalid(string name)
        => new TypeMetadata(null, name, [], [], [], [], [], [], false, true) { IsInvalid = true };

    public override string ToString()
    {
        if (!IsGeneric)
            return Name;

        var sb = new StringBuilder();

        if (genericArguments.Any(x => x is not TypeArgumentMetadata))
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
        => genericArguments.Add(typeArgumentMetadata);

    public void AddInterface(InterfaceMetadata interfaceMetadata)
        => interfaces.Add(interfaceMetadata);

    public void AddField(FieldMetadata field)
        => fields.Add(field);

    public AggregateMetadata GetFields(string name)
        => new AggregateMetadata(fields.Where(f => f.Name == name));

    public void AddProperty(PropertyMetadata property)
        => properties.Add(property);

    public AggregateMetadata GetProperties(string name)
        => new AggregateMetadata(properties.Where(f => f.Name == name));

    public void AddConstructor(ConstructorMetadata constructor)
        => constructors.Add(constructor);

    public ConstructorMetadata? GetConstructor(IEnumerable<ITypeMetadata> parameters)
        => constructors.FirstOrDefault(c => c.Type.ParameterTypes.SequenceEqual(parameters));

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

    public bool IsValueType { get; }

    public TypeLayout? Layout { get; set; }

    public INamespaceMetadata? Namespace { get; set; }

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