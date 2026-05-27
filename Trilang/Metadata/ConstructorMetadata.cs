namespace Trilang.Metadata;

public class ConstructorMetadata : IFunctionMetadata, ITypedMetadata
{
    public const string Name = "<>_ctor";

    public ConstructorMetadata(
        SourceLocation? definition,
        ITypeMetadata declaringType,
        AccessModifierMetadata accessModifier,
        IReadOnlyList<ParameterMetadata> parameters,
        FunctionTypeMetadata type)
    {
        Definition = definition;
        DeclaringType = declaringType;
        AccessModifier = accessModifier;
        Parameters = parameters;
        Type = type;
    }

    public override string ToString()
        => $"{Name}: {Type}";

    public void Freeze()
    {
        foreach (var parameter in Parameters)
            parameter.Freeze();
    }

    public SourceLocation? Definition { get; }

    public bool IsInvalid => false;

    public ITypeMetadata DeclaringType { get; }

    public AccessModifierMetadata AccessModifier { get; }

    public IReadOnlyList<ParameterMetadata> Parameters { get; }

    public FunctionTypeMetadata Type { get; }

    ITypeMetadata ITypedMetadata.Type => Type;
}