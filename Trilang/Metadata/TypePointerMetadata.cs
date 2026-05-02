namespace Trilang.Metadata;

// TODO: anonymous type?
public class TypePointerMetadata : ITypeMetadata
{
    public TypePointerMetadata(ITypeMetadata type)
        => Type = type;

    public override string ToString()
        => $"{Type}*";

    public IMetadata? GetMember(string name)
        => Type.GetMember(name);

    public bool IsInvalid
        => false;

    public SourceLocation? Definition
        => Type.Definition;

    public bool IsValueType
        => true;

    public TypeLayout? Layout { get; set; }

    public INamespaceMetadata? Namespace { get; set; }

    public ITypeMetadata Type { get; }
}