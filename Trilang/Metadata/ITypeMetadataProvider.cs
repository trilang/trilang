namespace Trilang.Metadata;

public interface ITypeMetadataProvider
{
    ITypeMetadata? GetType(string name);
    bool DefineType(string name, ITypeMetadata type);
    ITypeMetadataProvider CreateChild();
}