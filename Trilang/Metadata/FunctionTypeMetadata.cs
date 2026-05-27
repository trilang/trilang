using Trilang.Metadata.Aggregate;

namespace Trilang.Metadata;

public class FunctionTypeMetadata : IAnonymousTypeMetadata
{
    public const string FunctionField = "function";
    public const string ContextField = "context";

    private readonly HashSet<FieldMetadata> fields;
    private readonly List<ITypeMetadata> parameterTypes;
    private bool isFrozen;

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
    {
        EnsureNotFrozen();
        fields.Add(field);
    }

    public AggregateMetadata GetMembers(string name)
    {
        var field = fields.FirstOrDefault(f => f.Name == name);

        return field is null
            ? AggregateMetadata.Empty
            : new AggregateMetadata([field]);
    }

    public void MarkAsInvalid()
    {
        EnsureNotFrozen();
        IsInvalid = true;
    }

    public void Freeze()
    {
        isFrozen = true;

        foreach (var field in fields)
            field.Freeze();
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
        => true;

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

    public IReadOnlyCollection<FieldMetadata> Fields => fields;

    public IReadOnlyList<ITypeMetadata> ParameterTypes => parameterTypes;

    public ITypeMetadata ReturnType { get; }
}