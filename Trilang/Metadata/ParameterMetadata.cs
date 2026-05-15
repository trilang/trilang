namespace Trilang.Metadata;

public class ParameterMetadata : IMetadata
{
    private bool isFrozen;

    public ParameterMetadata(SourceLocation? definition, string name, ITypeMetadata type)
    {
        Definition = definition;
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

    public string Name { get; }

    public ITypeMetadata Type { get; }
}