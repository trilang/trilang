namespace Trilang.Metadata;

public class InvalidMemberMetadata : IMetadata
{
    public InvalidMemberMetadata(string name)
        => Name = name;

    public SourceLocation? Definition => null;

    public bool IsInvalid => true;

    public string Name { get; }

    public ITypeMetadata Type => TypeMetadata.InvalidType;
}