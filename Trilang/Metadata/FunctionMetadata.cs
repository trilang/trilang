namespace Trilang.Metadata;

public class FunctionMetadata : IFunctionMetadata
{
    private readonly List<ParameterMetadata> parameters;

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

    public override string ToString()
        => $"{Name}: {Type}";

    public void AddParameter(ParameterMetadata parameter)
        => parameters.Add(parameter);

    public void MarkAsInvalid()
        => IsInvalid = true;

    public SourceLocation? Definition { get; }

    public bool IsInvalid { get; private set; }

    public AccessModifierMetadata AccessModifier { get; }

    public string Name { get; }

    public IReadOnlyList<ParameterMetadata> Parameters => parameters;

    public FunctionTypeMetadata Type { get; set; }

    public INamespaceMetadata? Namespace { get; set; }
}