using Trilang.Parsing.Ast;

namespace Trilang.Metadata;

public class TypeMetadata : IMetadata, IEquatable<TypeMetadata>
{
    public static readonly TypeMetadata Void = new TypeMetadata("void");
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

    private readonly HashSet<FieldMetadata> fields;
    private readonly HashSet<MethodMetadata> methods;

    public TypeMetadata(string name) : this(name, [], [])
    {
    }

    public TypeMetadata(string name, IEnumerable<FieldMetadata> fields, IEnumerable<MethodMetadata> methods)
    {
        Name = name;
        this.fields = new HashSet<FieldMetadata>(fields);
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

        return Name == other.Name &&
               Fields.SequenceEqual(other.Fields) &&
               Methods.SequenceEqual(other.Methods);
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

    public void AddField(FieldMetadata field)
    {
        if (!fields.Add(field))
            throw new ArgumentException($"Field with name {field.Name} already exists in type {Name}");
    }

    public void AddMethod(MethodMetadata method)
    {
        if (!methods.Add(method))
            throw new ArgumentException($"Method with name {method.Name} already exists in type {Name}");
    }

    public string Name { get; }

    public IReadOnlyCollection<FieldMetadata> Fields => fields;

    public IReadOnlyCollection<MethodMetadata> Methods => methods;
}