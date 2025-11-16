namespace Trilang.Metadata;

public interface IFunctionMetadata : IMetadata
{
    FunctionTypeMetadata Type { get; }
}