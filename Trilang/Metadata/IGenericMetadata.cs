namespace Trilang.Metadata;

public interface IGenericMetadata : ITypeMetadata
{
    IReadOnlyList<ITypeMetadata> GenericArguments { get; }

    bool IsGeneric { get; }
}