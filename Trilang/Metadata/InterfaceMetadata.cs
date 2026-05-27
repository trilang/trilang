using Trilang.Metadata.Aggregate;

namespace Trilang.Metadata;

public class InterfaceMetadata : IAnonymousTypeMetadata
{
    private readonly List<InterfacePropertyMetadata> properties;
    private readonly List<InterfaceMethodMetadata> methods;
    private bool isFrozen;

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
    {
        EnsureNotFrozen();
        properties.Add(property);
    }

    public AggregateMetadata GetProperties(string name)
        => new AggregateMetadata(properties.Where(f => f.Name == name));

    public void AddMethod(InterfaceMethodMetadata method)
    {
        EnsureNotFrozen();
        methods.Add(method);
    }

    public AggregateMetadata GetMethods(string name)
        => new AggregateMetadata(methods.Where(m => m.Name == name));

    public AggregateMetadata GetMembers(string name)
        => GetProperties(name)
            .Combine(GetMethods(name));

    public void MarkAsInvalid()
    {
        EnsureNotFrozen();
        IsInvalid = true;
    }

    public void Freeze()
    {
        isFrozen = true;

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

    public IReadOnlyList<InterfacePropertyMetadata> Properties
        => properties;

    public IReadOnlyList<InterfaceMethodMetadata> Methods
        => methods;
}