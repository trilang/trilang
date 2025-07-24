namespace Trilang.Metadata;

public interface ITypeMetadataProvider
{
    ITypeMetadata? GetType(string name);

    bool DefineType(string name, ITypeMetadata type);

    T GetOrDefine<T>(T type) where T : ITypeMetadata;

    ITypeMetadataProvider CreateChild();
}