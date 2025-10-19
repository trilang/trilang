namespace Trilang.Metadata;

public class TupleMetadata : ITypeMetadata, IEquatable<TupleMetadata>
{
    private readonly List<ITypeMetadata> types;
    private readonly List<FieldMetadata> fields;
    private readonly List<PropertyMetadata> properties;
    private readonly List<MethodMetadata> methods;

    public TupleMetadata(SourceLocation? definition) : this(definition, [])
    {
    }

    public TupleMetadata(SourceLocation? definition, IEnumerable<ITypeMetadata> types)
    {
        Definition = definition;
        this.types = [];
        this.fields = [];
        this.properties = [];
        this.methods = [];

        foreach (var type in types)
            AddType(type);
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

        return types.SequenceEqual(other.types);
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
    {
        var name = types.Count.ToString();

        types.Add(type);
        fields.Add(new FieldMetadata(this, $"<>_{name}", type));

        // TODO: add in ctor?
        var propertyMetadata = new PropertyMetadata(null, this, name, type, AccessModifierMetadata.Public);
        properties.Add(propertyMetadata);
        methods.Add(propertyMetadata.Getter!);
    }

    public IMetadata? GetMember(string name)
        => GetProperty(name) ??
           GetMethod(name) ??
           GetField(name) as IMetadata;

    public FieldMetadata? GetField(string name)
        => fields.FirstOrDefault(f => f.Name == name);

    public PropertyMetadata? GetProperty(string name)
        => properties.FirstOrDefault(f => f.Name == name);

    public MethodMetadata? GetMethod(string name)
        => methods.FirstOrDefault(f => f.Name == name);

    public bool IsInvalid { get; }

    public SourceLocation? Definition { get; }

    public bool IsValueType
        => true;

    public TypeLayout? Layout { get; set; }

    public IReadOnlyList<ITypeMetadata> Types
        => types;

    public IReadOnlyList<FieldMetadata> Fields
        => fields;

    public IReadOnlyList<PropertyMetadata> Properties
        => properties;

    public IReadOnlyList<MethodMetadata> Methods
        => methods;
}