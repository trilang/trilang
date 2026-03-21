namespace Trilang.Metadata;

public class FieldMetadata : IMetadata
{
    public FieldMetadata(
        ITypeMetadata declaringType,
        string name,
        ITypeMetadata type)
    {
        DeclaringType = declaringType;
        Name = name;
        Type = type;
    }

    public override string ToString()
        => $"{Name}: {Type}";

    public SourceLocation? Definition => null;

    public bool IsInvalid => false;

    public ITypeMetadata DeclaringType { get; }

    public string Name { get; }

    public ITypeMetadata Type { get; }
}