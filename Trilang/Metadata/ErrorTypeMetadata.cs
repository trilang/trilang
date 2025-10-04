namespace Trilang.Metadata;

// TODO: `never` type?
public class ErrorTypeMetadata : ITypeMetadata
{
    public readonly static ErrorTypeMetadata Instance = new ErrorTypeMetadata();

    public bool IsValueType => false;

    public TypeLayout? Layout { get; set; }

    public IMetadata? GetMember(string name)
        => throw new NotImplementedException();
}