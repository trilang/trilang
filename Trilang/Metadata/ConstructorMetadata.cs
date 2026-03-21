namespace Trilang.Metadata;

public class ConstructorMetadata : IFunctionMetadata
{
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

    public static ConstructorMetadata Invalid()
        => new ConstructorMetadata(
            null,
            TypeMetadata.InvalidType,
            AccessModifierMetadata.Public,
            [],
            FunctionTypeMetadata.Invalid());

    public override string ToString()
        => $"ctor: {Type}";

    public SourceLocation? Definition { get; }

    public bool IsInvalid => false;

    public ITypeMetadata DeclaringType { get; }

    public string Name => "ctor";

    public AccessModifierMetadata AccessModifier { get; }

    public IReadOnlyList<ParameterMetadata> Parameters { get; }

    public FunctionTypeMetadata Type { get; }
}