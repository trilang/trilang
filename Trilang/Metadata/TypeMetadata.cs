namespace Trilang.Metadata;

public class TypeMetadata : ITypeMetadata, IEquatable<TypeMetadata>
{
    public static readonly TypeMetadata Void = new TypeMetadata("void");
    public static readonly TypeMetadata Null = new TypeMetadata("null");
    public static readonly TypeMetadata I8 = new TypeMetadata("i8");
    public static readonly TypeMetadata I16 = new TypeMetadata("i16");
    public static readonly TypeMetadata I32 = new TypeMetadata("i32");
    public static readonly TypeMetadata I64 = new TypeMetadata("i64");
    public static readonly TypeMetadata U8 = new TypeMetadata("u8");
    public static readonly TypeMetadata U16 = new TypeMetadata("u16");
    public static readonly TypeMetadata U32 = new TypeMetadata("u32");
    public static readonly TypeMetadata U64 = new TypeMetadata("u64");
    public static readonly TypeMetadata F32 = new TypeMetadata("f32");
    public static readonly TypeMetadata F64 = new TypeMetadata("f64");
    public static readonly TypeMetadata Char = new TypeMetadata("char");
    public static readonly TypeMetadata Bool = new TypeMetadata("bool");
    public static readonly TypeMetadata String = new TypeMetadata("string");

    private readonly HashSet<InterfaceMetadata> interfaces;
    private readonly HashSet<FieldMetadata> fields;
    private readonly HashSet<ConstructorMetadata> constructors;
    private readonly HashSet<MethodMetadata> methods;

    public TypeMetadata(string name) : this(name, [], [], [], [])
    {
    }

    public TypeMetadata(
        string name,
        IEnumerable<InterfaceMetadata> interfaces,
        IEnumerable<FieldMetadata> fields,
        IEnumerable<ConstructorMetadata> constructors,
        IEnumerable<MethodMetadata> methods)
    {
        Name = name;
        this.interfaces = new HashSet<InterfaceMetadata>(interfaces);
        this.fields = new HashSet<FieldMetadata>(fields);
        this.constructors = new HashSet<ConstructorMetadata>(constructors);
        this.methods = new HashSet<MethodMetadata>(methods);
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

        return Name == other.Name;
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
        => Name;

    public void AddInterface(InterfaceMetadata interfaceMetadata)
        => interfaces.Add(interfaceMetadata);

    public InterfaceMetadata? GetInterface(string name)
        => interfaces.FirstOrDefault(i => i.Name == name);

    public FieldMetadata? GetField(string name)
        => fields.FirstOrDefault(f => f.Name == name);

    public void AddField(FieldMetadata field)
    {
        if (!fields.Add(field))
            throw new ArgumentException($"Field with name {field.Name} already exists in type {Name}");
    }

    public ConstructorMetadata? GetConstructor(IEnumerable<ITypeMetadata> parameters)
        => constructors.FirstOrDefault(c => c.ParameterTypes.SequenceEqual(parameters));

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

    public IReadOnlyCollection<InterfaceMetadata> Interfaces => interfaces;

    public IReadOnlyCollection<FieldMetadata> Fields => fields;

    public IReadOnlyCollection<ConstructorMetadata> Constructors => constructors;

    public IReadOnlyCollection<MethodMetadata> Methods => methods;
}