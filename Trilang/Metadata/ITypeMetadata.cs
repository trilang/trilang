namespace Trilang.Metadata;

public interface ITypeMetadata : IMetadata
{
    bool IsValueType { get; }
}