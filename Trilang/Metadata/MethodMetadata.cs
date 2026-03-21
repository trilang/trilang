namespace Trilang.Metadata;

public class MethodMetadata : IFunctionMetadata
{
    public MethodMetadata(
        SourceLocation? definition,
        ITypeMetadata declaringType,
        AccessModifierMetadata accessModifier,
        bool isStatic,
        string name,
        IReadOnlyList<ParameterMetadata> parameters,
        FunctionTypeMetadata type)
    {
        Definition = definition;
        DeclaringType = declaringType;
        AccessModifier = accessModifier;
        IsStatic = isStatic;
        Name = name;
        Parameters = parameters;
        Type = type;
    }

    public override string ToString()
        => $"{Name}: {Type}";

    public void MarkAsInvalid()
        => IsInvalid = true;

    public SourceLocation? Definition { get; }

    public bool IsInvalid { get; private set; }

    public ITypeMetadata DeclaringType { get; }

    public AccessModifierMetadata AccessModifier { get; }

    public bool IsStatic { get; }

    public string Name { get; }

    public IReadOnlyList<ParameterMetadata> Parameters { get; }

    public FunctionTypeMetadata Type { get; }
}