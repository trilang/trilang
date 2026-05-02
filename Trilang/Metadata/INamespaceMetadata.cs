namespace Trilang.Metadata;

public interface INamespaceMetadata : IMetadata
{
    void AddType(ITypeMetadata type);

    void AddFunction(FunctionMetadata function);

    IEnumerable<FunctionMetadata> FindFunction(string name);

    IReadOnlyCollection<ITypeMetadata> Types { get; }
}