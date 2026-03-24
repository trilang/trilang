using Trilang.Metadata;

namespace Trilang.Semantics.Providers;

public interface IMetadataProvider
{
    QueryTypesResult QueryTypes(Query query);

    void DefineType(ITypeMetadata type);

    IReadOnlyList<FunctionMetadata> FindFunctions(string name);

    void AddFunction(FunctionMetadata function);
}