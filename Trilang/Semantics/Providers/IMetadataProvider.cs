using Trilang.Metadata;

namespace Trilang.Semantics.Providers;

public interface IMetadataProvider
{
    // TODO: introduce query?
    IReadOnlyList<ITypeMetadata> FindTypes(string name);

    bool DefineType(string name, ITypeMetadata type);

    IReadOnlyList<FunctionMetadata> FindFunctions(string name);

    void AddFunction(FunctionMetadata function);
}