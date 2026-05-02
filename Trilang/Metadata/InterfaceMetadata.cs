namespace Trilang.Metadata;

public class InterfaceMetadata : IAnonymousTypeMetadata
{
    private readonly List<InterfacePropertyMetadata> properties;
    private readonly List<InterfaceMethodMetadata> methods;

    public InterfaceMetadata(SourceLocation? definition) : this(definition, [], [])
    {
    }

    public InterfaceMetadata(
        SourceLocation? definition,
        IEnumerable<InterfacePropertyMetadata> properties,
        IEnumerable<InterfaceMethodMetadata> methods)
    {
        Definition = definition;
        this.properties = [..properties];
        this.methods = [..methods];
    }

    public static InterfaceMetadata Invalid()
        => new InterfaceMetadata(null, [], []) { IsInvalid = true };

    public override string ToString()
    {
        var propertyNames = properties.Select(f => $"{f.Name}: {f.Type};");
        var methodNames = methods.Select(m => $"{m.Name}({string.Join(", ", m.Type.ParameterTypes)}): {m.Type.ReturnType};");

        var combinedSignatures = propertyNames.Concat(methodNames).ToList();

        return combinedSignatures.Count != 0
            ? $"{{ {string.Join(" ", combinedSignatures)} }}"
            : "{ }";
    }

    public void AddProperty(InterfacePropertyMetadata property)
        => properties.Add(property);

    public AggregateMetadata GetProperties(string name)
        => new AggregateMetadata(properties.Where(f => f.Name == name));

    public void AddMethod(InterfaceMethodMetadata method)
        => methods.Add(method);

    public AggregateMetadata GetMethods(string name)
        => new AggregateMetadata(methods.Where(m => m.Name == name));

    public IMetadata? GetMember(string name)
    {
        var aggregate = GetProperties(name)
            .Combine(GetMethods(name));

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

    public IReadOnlyList<InterfacePropertyMetadata> Properties
        => properties;

    public IReadOnlyList<InterfaceMethodMetadata> Methods
        => methods;
}