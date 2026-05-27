namespace Trilang.Metadata;

public class PropertyMetadata : ITypedMetadata
{
    private bool isFrozen;

    public PropertyMetadata(
        SourceLocation? definition,
        ITypeMetadata declaringType,
        string name,
        ITypeMetadata type,
        MethodMetadata? getter,
        MethodMetadata? setter)
    {
        Definition = definition;
        DeclaringType = declaringType;
        Name = name;
        Type = type;
        Getter = getter;
        Setter = setter;
    }

    public override string ToString()
        => $"{Name}: {Type}";

    public void MarkAsInvalid()
    {
        EnsureNotFrozen();
        IsInvalid = true;
    }

    public void Freeze()
    {
        isFrozen = true;

        Getter?.Freeze();
        Setter?.Freeze();
    }

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

    public ITypeMetadata DeclaringType { get; }

    public string Name { get; }

    public MethodMetadata? Getter { get; }

    public MethodMetadata? Setter { get; }

    public ITypeMetadata Type { get; }
}