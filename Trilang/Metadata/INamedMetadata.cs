namespace Trilang.Metadata;

public interface INamedMetadata : ITypeMetadata
{
    AccessModifierMetadata AccessModifier { get; }

    string Name { get; }

    // TODO: promote to IMetadata?
    bool IsCompilerGenerated { get; }
}