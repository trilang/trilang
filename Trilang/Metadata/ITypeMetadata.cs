namespace Trilang.Metadata;

public interface ITypeMetadata : IMetadata
{
    bool IsValueType { get; }

    TypeLayout? Layout { get; set; }

    INamespaceMetadata? Namespace { get; set; }

    AggregateMetadata GetMembers(string name);
}