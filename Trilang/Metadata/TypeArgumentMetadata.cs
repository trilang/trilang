namespace Trilang.Metadata;

public class TypeArgumentMetadata : ITypeMetadata
{
    private bool isFrozen;

    public TypeArgumentMetadata(SourceLocation? definition, string name)
    {
        Definition = definition;
        Name = name;
    }

    public static TypeArgumentMetadata Invalid(string name)
        => new TypeArgumentMetadata(null, name) { IsInvalid = true };

    public override int GetHashCode()
        => HashCode.Combine(Name);

    public override string ToString()
        => Name;

    public AggregateMetadata GetMembers(string name)
        => AggregateMetadata.Empty;

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
        => throw new NotSupportedException();

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
        get => throw new NotSupportedException();
        set => throw new NotSupportedException();
    }

    public string Name { get; }
}