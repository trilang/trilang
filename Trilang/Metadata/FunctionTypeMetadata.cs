namespace Trilang.Metadata;

public class FunctionTypeMetadata : IAnonymousTypeMetadata
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