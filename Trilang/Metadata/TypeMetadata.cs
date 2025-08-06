using System.Text;

namespace Trilang.Metadata;

// TODO: immutable metadata
public class TypeMetadata : ITypeMetadata, IEquatable<TypeMetadata>
{
    public static readonly TypeMetadata Void = new TypeMetadata("void", [], [], [], [], [], [], true);
    public static readonly TypeMetadata Null = new TypeMetadata("null", [], [], [], [], [], [], true);
    public static readonly TypeMetadata I8 = new TypeMetadata("i8", [], [], [], [], [], [], true);
    public static readonly TypeMetadata I16 = new TypeMetadata("i16", [], [], [], [], [], [], true);
    public static readonly TypeMetadata I32 = new TypeMetadata("i32", [], [], [], [], [], [], true);
    public static readonly TypeMetadata I64 = new TypeMetadata("i64", [], [], [], [], [], [], true);
    public static readonly TypeMetadata U8 = new TypeMetadata("u8", [], [], [], [], [], [], true);
    public static readonly TypeMetadata U16 = new TypeMetadata("u16", [], [], [], [], [], [], true);
    public static readonly TypeMetadata U32 = new TypeMetadata("u32", [], [], [], [], [], [], true);
    public static readonly TypeMetadata U64 = new TypeMetadata("u64", [], [], [], [], [], [], true);
    public static readonly TypeMetadata F32 = new TypeMetadata("f32", [], [], [], [], [], [], true);
    public static readonly TypeMetadata F64 = new TypeMetadata("f64", [], [], [], [], [], [], true);
    public static readonly TypeMetadata Char = new TypeMetadata("char", [], [], [], [], [], [], true);
    public static readonly TypeMetadata Bool = new TypeMetadata("bool", [], [], [], [], [], [], true);
    public static readonly TypeMetadata String = new TypeMetadata("string", [], [], [], [], [], [], false);

    private readonly List<ITypeMetadata> genericArguments;
    private readonly HashSet<InterfaceMetadata> interfaces;
    private readonly HashSet<FieldMetadata> fields;
    private readonly HashSet<PropertyMetadata> properties;
    private readonly HashSet<ConstructorMetadata> constructors;
    private readonly HashSet<MethodMetadata> methods;

    private TypeMetadata(
        string name,
        IEnumerable<ITypeMetadata> genericArguments,
        IEnumerable<InterfaceMetadata> interfaces,
        IEnumerable<FieldMetadata> fields,
        IEnumerable<PropertyMetadata> properties,
        IEnumerable<ConstructorMetadata> constructors,
        IEnumerable<MethodMetadata> methods,
        bool isValueType)
    {
        Name = name;
        this.genericArguments = [..genericArguments];
        this.interfaces = [..interfaces];
        this.fields = [..fields];
        this.properties = [..properties];
        this.constructors = [..constructors];
        this.methods = [..methods];
        IsValueType = isValueType;
    }

    public TypeMetadata(string name) : this(name, [], [], [], [], [], [], false)
    {
    }

    public TypeMetadata(
        string name,
        IEnumerable<ITypeMetadata> genericArguments,
        IEnumerable<InterfaceMetadata> interfaces,
        IEnumerable<FieldMetadata> fields,
        IEnumerable<PropertyMetadata> properties,
        IEnumerable<ConstructorMetadata> constructors,
        IEnumerable<MethodMetadata> methods) : this(name, genericArguments, interfaces, fields, properties, constructors, methods, false)
    {
    }

    public static bool operator ==(TypeMetadata? left, TypeMetadata? right)
        => Equals(left, right);

    public static bool operator !=(TypeMetadata? left, TypeMetadata? right)
        => !Equals(left, right);

    public bool Equals(TypeMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name &&
               genericArguments.SequenceEqual(other.genericArguments);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((TypeMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name);

    public override string ToString()
    {
        if (genericArguments.Count == 0)
            return Name;

        var sb = new StringBuilder();
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

    public PropertyMetadata? GetProperty(string name)
        => properties.FirstOrDefault(f => f.Name == name);

    public void AddProperty(PropertyMetadata property)
    {
        if (!properties.Add(property))
            throw new ArgumentException($"Property with name {property.Name} already exists in type {Name}");
    }

    public ConstructorMetadata? GetConstructor(IEnumerable<ITypeMetadata> parameters)
        => constructors.FirstOrDefault(c => c.TypeMetadata.ParameterTypes.SequenceEqual(parameters));

    public void AddConstructor(ConstructorMetadata constructor)
    {
        if (!constructors.Add(constructor))
            throw new ArgumentException($"Constructor already exists in type {Name}");
    }

    public MethodMetadata? GetMethod(string name)
        => methods.FirstOrDefault(m => m.Name == name);

    public void AddMethod(MethodMetadata method)
    {
        if (!methods.Add(method))
            throw new ArgumentException($"Method with name {method.Name} already exists in type {Name}");
    }

    public string Name { get; }

    public IReadOnlyCollection<ITypeMetadata> GenericArguments => genericArguments;

    public IReadOnlyCollection<InterfaceMetadata> Interfaces => interfaces;

    public IReadOnlyCollection<PropertyMetadata> Properties => properties;

    public IReadOnlyCollection<FieldMetadata> Fields => fields;

    public IReadOnlyCollection<ConstructorMetadata> Constructors => constructors;

    public IReadOnlyCollection<MethodMetadata> Methods => methods;

    public bool IsValueType { get; }
}