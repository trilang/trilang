namespace Trilang.Metadata;

public class InvalidMemberMetadata : ITypedMetadata
{
    public static readonly InvalidMemberMetadata Instance = new InvalidMemberMetadata();

    private InvalidMemberMetadata()
    {
    }

    public SourceLocation? Definition
        => null;

    public bool IsInvalid
        => true;

    public ITypeMetadata Type
        => TypeMetadata.InvalidType;

    public void Freeze()
    {
    }
}