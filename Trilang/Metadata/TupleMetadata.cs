namespace Trilang.Metadata;

public class TupleMetadata : IAnonymousTypeMetadata, IEquatable<TupleMetadata>
{
    private readonly List<ITypeMetadata> types;
    private readonly List<FieldMetadata> fields;
    private readonly List<PropertyMetadata> properties;
    private readonly List<MethodMetadata> methods;

    public TupleMetadata(SourceLocation? definition)
    {
        Definition = definition;

        types = [];
        fields = [];
        properties = [];
        methods = [];
    }

    public static bool operator ==(TupleMetadata? left, TupleMetadata? right)
        => Equals(left, right);

    public static bool operator !=(TupleMetadata? left, TupleMetadata? right)
        => !Equals(left, right);

    public bool Equals(TupleMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        if (IsInvalid || other.IsInvalid)
            return false;

        return types.SequenceEqual(other.types) &&
               Equals(Namespace, other.Namespace);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((TupleMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(types);

    public override string ToString()
        => $"({string.Join(", ", types)})";

    public void AddType(ITypeMetadata type)
        => types.Add(type);

    public IMetadata? GetMember(string name)
        => GetProperty(name) ??
           GetMethod(name) ??
           GetField(name) as IMetadata;

    public FieldMetadata? GetField(string name)
        => fields.FirstOrDefault(f => f.Name == name);

    public void AddField(FieldMetadata field)
        => fields.Add(field);

    public PropertyMetadata? GetProperty(string name)
        => properties.FirstOrDefault(f => f.Name == name);

    public void AddProperty(PropertyMetadata property)
        => properties.Add(property);

    public MethodMetadata? GetMethod(string name)
        => methods.FirstOrDefault(f => f.Name == name);

    public void AddMethod(MethodMetadata method)
        => methods.Add(method);

    public void MarkAsInvalid()
        => IsInvalid = true;

    public bool IsInvalid { get; private set; }

    public SourceLocation? Definition { get; }

    public bool IsValueType
        => true;

    public TypeLayout? Layout { get; set; }

    public NamespaceMetadata? Namespace { get; set; }

    public IReadOnlyList<ITypeMetadata> Types
        => types;

    public IReadOnlyList<FieldMetadata> Fields
        => fields;

    public IReadOnlyList<PropertyMetadata> Properties
        => properties;

    public IReadOnlyList<MethodMetadata> Methods
        => methods;
}