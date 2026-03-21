namespace Trilang.Metadata;

public class PropertyMetadata : IMetadata
{
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
        => IsInvalid = true;

    public SourceLocation? Definition { get; }

    public bool IsInvalid { get; private set; }

    public ITypeMetadata DeclaringType { get; }

    public string Name { get; }

    public MethodMetadata? Getter { get; }

    public MethodMetadata? Setter { get; }

    public ITypeMetadata Type { get; }
}