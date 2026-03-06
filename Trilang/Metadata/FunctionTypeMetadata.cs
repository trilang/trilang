namespace Trilang.Metadata;

public class FunctionTypeMetadata : IAnonymousTypeMetadata, IEquatable<FunctionTypeMetadata>
{
    public const string FunctionField = "function";
    public const string ContextField = "context";

    private readonly HashSet<FieldMetadata> fields;
    private readonly List<ITypeMetadata> parameterTypes;

    public FunctionTypeMetadata(
        SourceLocation? definition,
        IEnumerable<ITypeMetadata> parameterTypes,
        ITypeMetadata returnType)
    {
        fields = [];
        Definition = definition;
        this.parameterTypes = [.. parameterTypes];
        ReturnType = returnType;
    }

    public static FunctionTypeMetadata Invalid()
        => new FunctionTypeMetadata(null, [], TypeMetadata.InvalidType)
        {
            IsInvalid = true
        };

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

        if (IsInvalid || other.IsInvalid)
            return false;

        return parameterTypes.SequenceEqual(other.parameterTypes) &&
               ReturnType.Equals(other.ReturnType) &&
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

        return Equals((FunctionTypeMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(ParameterTypes, ReturnType);

    public override string ToString()
        => $"({string.Join(", ", parameterTypes)}) => {ReturnType}";

    public void AddField(FieldMetadata field)
        => fields.Add(field);

    public IMetadata? GetMember(string name)
        => fields.FirstOrDefault(f => f.Name == name);

    public void MarkAsInvalid()
        => IsInvalid = true;

    public bool IsInvalid { get; private set; }

    public SourceLocation? Definition { get; }

    public bool IsValueType
        => true;

    public TypeLayout? Layout { get; set; }

    public NamespaceMetadata? Namespace { get; set; }

    public IReadOnlyCollection<FieldMetadata> Fields => fields;

    public IReadOnlyList<ITypeMetadata> ParameterTypes => parameterTypes;

    public ITypeMetadata ReturnType { get; }
}