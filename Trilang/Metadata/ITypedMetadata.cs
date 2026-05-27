namespace Trilang.Metadata;

public interface ITypedMetadata : IMetadata
{
    ITypeMetadata Type { get; }
}