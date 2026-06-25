namespace Trilang.Metadata;

public class DiscriminatedUnionMetadata : IAnonymousTypeMetadata
{
    private readonly List<ITypeMetadata> types;
    private bool isFrozen;

    public DiscriminatedUnionMetadata(SourceLocation? definition, IEnumerable<ITypeMetadata> types)
    {
        Definition = definition;
        this.types = [.. types];
    }

    public override string ToString()
        => string.Join(" | ", types);

    public AggregateMetadata GetMembers(string name)
        => AggregateMetadata.Empty;

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

    public bool IsInvalid
    {
        get;
        private set
        {
            EnsureNotFrozen();
            field = value;
        }
    }

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

    public IReadOnlyList<ITypeMetadata> Types
        => types;
}