namespace Trilang.Metadata;

public interface INamedMetadata : ITypeMetadata
{
    string Name { get; }

    // TODO: promote to IMetadata?
    bool IsCompilerGenerated { get; }
}