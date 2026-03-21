namespace Trilang.Metadata;

public class InterfaceMethodMetadata : IFunctionMetadata
{
    public InterfaceMethodMetadata(
        SourceLocation? definition,
        InterfaceMetadata declaringType,
        string name,
        FunctionTypeMetadata type)
    {
        Definition = definition;
        DeclaringType = declaringType;
        Name = name;
        Type = type;
    }

    public override string ToString()
        => $"{Name}: {Type}";

    public void MarkAsInvalid()
        => IsInvalid = true;

    public SourceLocation? Definition { get; }

    public bool IsInvalid { get; private set; }

    public InterfaceMetadata DeclaringType { get; }

    public string Name { get; }

    public FunctionTypeMetadata Type { get; }
}