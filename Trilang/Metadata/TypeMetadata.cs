using System.Text;

namespace Trilang.Metadata;

// TODO: immutable metadata
public class TypeMetadata : ITypeMetadata, IEquatable<TypeMetadata>
{
    public static readonly TypeMetadata InvalidType = TypeMetadata.Invalid("<>_invalid_type");
    public static readonly TypeMetadata Void = new TypeMetadata(null, "void", [], [], [], [], [], [], true);
    public static readonly TypeMetadata Null = new TypeMetadata(null, "null", [], [], [], [], [], [], true);
    public static readonly TypeMetadata I8 = new TypeMetadata(null, "i8", [], [], [], [], [], [], true);
    public static readonly TypeMetadata I16 = new TypeMetadata(null, "i16", [], [], [], [], [], [], true);
    public static readonly TypeMetadata I32 = new TypeMetadata(null, "i32", [], [], [], [], [], [], true);
    public static readonly TypeMetadata I64 = new TypeMetadata(null, "i64", [], [], [], [], [], [], true);
    public static readonly TypeMetadata U8 = new TypeMetadata(null, "u8", [], [], [], [], [], [], true);
    public static readonly TypeMetadata U16 = new TypeMetadata(null, "u16", [], [], [], [], [], [], true);
    public static readonly TypeMetadata U32 = new TypeMetadata(null, "u32", [], [], [], [], [], [], true);
    public static readonly TypeMetadata U64 = new TypeMetadata(null, "u64", [], [], [], [], [], [], true);
    public static readonly TypeMetadata F32 = new TypeMetadata(null, "f32", [], [], [], [], [], [], true);
    public static readonly TypeMetadata F64 = new TypeMetadata(null, "f64", [], [], [], [], [], [], true);
    public static readonly TypeMetadata Char = new TypeMetadata(null, "char", [], [], [], [], [], [], true);
    public static readonly TypeMetadata Bool = new TypeMetadata(null, "bool", [], [], [], [], [], [], true);
    public static readonly TypeMetadata String = new TypeMetadata(null, "string", [], [], [], [], [], [], false);

    private readonly List<ITypeMetadata> genericArguments;
    private readonly HashSet<InterfaceMetadata> interfaces;
    private readonly List<FieldMetadata> fields;
    private readonly List<PropertyMetadata> properties;
    private readonly List<ConstructorMetadata> constructors;
    private readonly List<MethodMetadata> methods;

    private TypeMetadata(
        SourceLocation? definition,
        string name,
        IEnumerable<ITypeMetadata> genericArguments,
        IEnumerable<InterfaceMetadata> interfaces,
        IEnumerable<FieldMetadata> fields,
        IEnumerable<PropertyMetadata> properties,
        IEnumerable<ConstructorMetadata> constructors,
        IEnumerable<MethodMetadata> methods,
        bool isValueType)
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
    }

    public TypeMetadata(SourceLocation? definition, string name)
        : this(definition, name, [], [], [], [], [], [], false)
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
        IEnumerable<MethodMetadata> methods) : this(definition, name, genericArguments, interfaces, fields, properties, constructors, methods, false)
    {
    }

    public static TypeMetadata Invalid(string name)
        => new TypeMetadata(null, name) { IsInvalid = true };

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

        if (IsInvalid || other.IsInvalid)
            return false;

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

    public FieldMetadata? GetField(string name)
        => fields.FirstOrDefault(f => f.Name == name);

    public PropertyMetadata? GetProperty(string name)
        => properties.FirstOrDefault(f => f.Name == name);

    public void AddProperty(PropertyMetadata property)
        => properties.Add(property);

    public ConstructorMetadata? GetConstructor(IEnumerable<ITypeMetadata> parameters)
        => constructors.FirstOrDefault(c => c.Type.ParameterTypes.SequenceEqual(parameters));

    public void AddConstructor(ConstructorMetadata constructor)
        => constructors.Add(constructor);

    public FunctionGroupMetadata? GetMethod(string name)
        => methods.FirstOrDefault(m => m.Name == name)?.Group;

    public IEnumerable<MethodMetadata> GetMethods(string name, IEnumerable<ITypeMetadata> parameters)
        => methods.Where(m => m.Name == name && m.Type.ParameterTypes.SequenceEqual(parameters));

    public void AddMethod(MethodMetadata method)
        => methods.Add(method);

    public IMetadata? GetMember(string name)
        => GetProperty(name) ??
           GetMethod(name) ??
           GetField(name) as IMetadata;

    public void MarkAsInvalid()
        => IsInvalid = true;

    public bool IsInvalid { get; private set; }

    public SourceLocation? Definition { get; }

    public bool IsValueType { get; }

    public TypeLayout? Layout { get; set; }

    public string Name { get; }

    public IReadOnlyList<ITypeMetadata> GenericArguments => genericArguments;

    public IReadOnlyCollection<InterfaceMetadata> Interfaces => interfaces;

    public IReadOnlyList<PropertyMetadata> Properties => properties;

    public IReadOnlyList<FieldMetadata> Fields => fields;

    public IReadOnlyList<ConstructorMetadata> Constructors => constructors;

    public IReadOnlyList<MethodMetadata> Methods => methods;

    public bool IsGeneric => genericArguments.Count > 0;
}