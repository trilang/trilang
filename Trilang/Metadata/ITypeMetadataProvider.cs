namespace Trilang.Metadata;

public interface ITypeMetadataProvider
{
    ITypeMetadata? GetType(string name);

    bool DefineType(ITypeMetadata type);

    bool DefineType(string name, ITypeMetadata type);

    T GetOrDefine<T>(T type) where T : ITypeMetadata;

    void AddFunction(FunctionMetadata function);

    ITypeMetadataProvider CreateChild();

    IEnumerable<ITypeMetadata> Types { get; }

    IReadOnlyList<FunctionMetadata> Functions { get; }
}