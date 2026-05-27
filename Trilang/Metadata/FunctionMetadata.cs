namespace Trilang.Metadata;

public class FunctionMetadata : IFunctionMetadata, ITypedMetadata
{
    public static readonly FunctionMetadata Invalid;

    private readonly List<ParameterMetadata> parameters;
    private bool isFrozen;

    public FunctionMetadata(
        SourceLocation? definition,
        AccessModifierMetadata accessModifier,
        string name,
        IReadOnlyList<ParameterMetadata> parameters,
        FunctionTypeMetadata type)
    {
        Definition = definition;
        AccessModifier = accessModifier;
        Name = name;
        this.parameters = [..parameters];
        Type = type;
    }

    static FunctionMetadata()
    {
        Invalid = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "<>_invalid_function",
            [],
            FunctionTypeMetadata.Invalid());

        Invalid.MarkAsInvalid();
        Invalid.Freeze();
    }

    public override string ToString()
        => $"{Name}: {Type}";

    public void AddParameter(ParameterMetadata parameter)
    {
        EnsureNotFrozen();
        parameters.Add(parameter);
    }

    public void MarkAsInvalid()
    {
        EnsureNotFrozen();
        IsInvalid = true;
    }

    public void Freeze()
    {
        isFrozen = true;

        foreach (var parameter in parameters)
            parameter.Freeze();
    }

    private void EnsureNotFrozen()
    {
        if (isFrozen)
            throw new InvalidOperationException("Cannot modify frozen metadata.");
    }

    public SourceLocation? Definition { get; }

    public bool IsInvalid
    {
        get;
        private set
        {
            EnsureNotFrozen();
            field = value;
        }
    }

    public AccessModifierMetadata AccessModifier { get; }

    public string Name { get; }

    public IReadOnlyList<ParameterMetadata> Parameters => parameters;

    public FunctionTypeMetadata Type
    {
        get;
        set
        {
            EnsureNotFrozen();
            field = value;
        }
    }

    ITypeMetadata ITypedMetadata.Type => Type;

    public INamespaceMetadata? Namespace
    {
        get;
        set
        {
            EnsureNotFrozen();
            field = value;
        }
    }
}