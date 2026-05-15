namespace Trilang.Metadata;

// TODO: anonymous type?
public class TypePointerMetadata : ITypeMetadata
{
    private bool isFrozen;

    public TypePointerMetadata(ITypeMetadata type)
        => Type = type;

    public override string ToString()
        => $"{Type}*";

    public IMetadata? GetMember(string name)
        => Type.GetMember(name);

    public void Freeze()
        => isFrozen = true;

    private void EnsureNotFrozen()
    {
        if (isFrozen)
            throw new InvalidOperationException("Cannot modify frozen metadata.");
    }

    public bool IsInvalid
        => false;

    public SourceLocation? Definition
        => Type.Definition;

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