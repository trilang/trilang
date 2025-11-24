namespace Trilang.Metadata;

public interface IMetadataProvider
{
    ITypeMetadata? GetType(string name);

    bool DefineType(ITypeMetadata type);

    bool DefineType(string name, ITypeMetadata type);

    T GetOrDefine<T>(T type) where T : ITypeMetadata;

    void AddFunction(FunctionMetadata function);

    IMetadataProvider CreateChild();

    IEnumerable<ITypeMetadata> Types { get; }

    IReadOnlyList<FunctionMetadata> Functions { get; }
}