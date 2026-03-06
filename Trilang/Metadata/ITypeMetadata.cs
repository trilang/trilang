namespace Trilang.Metadata;

public interface ITypeMetadata : IMetadata
{
    bool IsValueType { get; }

    TypeLayout? Layout { get; set; }

    NamespaceMetadata? Namespace { get; set; }

    IMetadata? GetMember(string name);
}