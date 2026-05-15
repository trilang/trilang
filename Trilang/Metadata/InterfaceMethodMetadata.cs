namespace Trilang.Metadata;

public class InterfaceMethodMetadata : IFunctionMetadata
{
    private bool isFrozen;

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
    {
        EnsureNotFrozen();
        IsInvalid = true;
    }

    public void Freeze()
        => isFrozen = true;

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

    public InterfaceMetadata DeclaringType { get; }

    public string Name { get; }

    public FunctionTypeMetadata Type { get; }
}