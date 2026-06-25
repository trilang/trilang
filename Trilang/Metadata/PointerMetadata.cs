namespace Trilang.Metadata;

public class PointerMetadata : IAnonymousTypeMetadata
{
    private bool isFrozen;

    public PointerMetadata(SourceLocation? definition, ITypeMetadata type)
    {
        Definition = definition;
        Type = type;
    }

    public override string ToString()
        => Type switch
        {
            DiscriminatedUnionMetadata or FunctionTypeMetadata => $"({Type})*",
            _ => $"{Type}*",
        };

    public AggregateMetadata GetMembers(string name)
        => Type.GetMembers(name);

    public void Freeze()
        => isFrozen = true;

    private void EnsureNotFrozen()
    {
        if (isFrozen)
            throw new InvalidOperationException("Cannot modify frozen metadata.");
    }

    public void MarkAsInvalid()
        => IsInvalid = true;

    public bool IsInvalid { get; private set; }

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

    public ITypeMetadata Type { get; }
}