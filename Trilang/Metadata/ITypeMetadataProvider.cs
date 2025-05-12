namespace Trilang.Metadata;

public interface ITypeMetadataProvider
{
    ITypeMetadata? GetType(string name);
    bool DefineType(ITypeMetadata type);
    ITypeMetadataProvider CreateChild();
}