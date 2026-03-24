namespace Trilang.Metadata;

public interface IGenericMetadata : ITypeMetadata
{
    string Name { get; }

    IReadOnlyList<ITypeMetadata> GenericArguments { get; }

    bool IsGeneric { get; }
}