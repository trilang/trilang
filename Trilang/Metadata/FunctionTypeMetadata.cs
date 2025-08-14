namespace Trilang.Metadata;

public class FunctionTypeMetadata : ITypeMetadata, IEquatable<FunctionTypeMetadata>
{
    public const string FunctionField = "function";
    public const string ContextField = "context";

    private readonly List<FieldMetadata> fields;
    private readonly List<ITypeMetadata> parameterTypes;

    public FunctionTypeMetadata() : this([], null!)
    {
    }

    public FunctionTypeMetadata(IEnumerable<ITypeMetadata> parameterTypes, ITypeMetadata returnType)
    {
        // TODO: use property?
        fields =
        [
            // TODO: replace pointer with something else? introduce delegate type?
            new FieldMetadata(this, FunctionField, new TypePointerMetadata(TypeMetadata.Void)),
            new FieldMetadata(this, ContextField,
                new DiscriminatedUnionMetadata([new InterfaceMetadata(), TypeMetadata.Null])),
        ];

        this.parameterTypes = [..parameterTypes];
        ReturnType = returnType;
    }

    public static bool operator ==(FunctionTypeMetadata? left, FunctionTypeMetadata? right)
        => Equals(left, right);

    public static bool operator !=(FunctionTypeMetadata? left, FunctionTypeMetadata? right)
        => !Equals(left, right);

    public bool Equals(FunctionTypeMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return parameterTypes.SequenceEqual(other.parameterTypes) &&
               ReturnType.Equals(other.ReturnType);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((FunctionTypeMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(ParameterTypes, ReturnType);

    public override string ToString()
        => $"({string.Join(", ", parameterTypes)}) => {ReturnType}";

    public void AddParameter(ITypeMetadata parameter)
        => parameterTypes.Add(parameter);

    public IMetadata? GetMember(string name)
        => fields.FirstOrDefault(f => f.Name == name);

    public IReadOnlyList<FieldMetadata> Fields => fields;

    public FieldMetadata Function => fields[0];

    public FieldMetadata Context => fields[1];

    public IReadOnlyList<ITypeMetadata> ParameterTypes => parameterTypes;

    public ITypeMetadata ReturnType { get; set; }

    public bool IsValueType
        => true;

    public TypeLayout? Layout { get; set; }
}