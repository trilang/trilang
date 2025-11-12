namespace Trilang.Metadata;

public interface IHasFunctionType : IMetadata
{
    FunctionTypeMetadata Type { get; }
}