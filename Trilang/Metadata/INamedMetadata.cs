namespace Trilang.Metadata;

public interface INamedMetadata : ITypeMetadata
{
    string Name { get; }
}