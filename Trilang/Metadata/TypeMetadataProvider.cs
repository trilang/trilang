namespace Trilang.Metadata;

public class TypeMetadataProvider : ITypeMetadataProvider
{
    private readonly ITypeMetadataProvider parent;
    private readonly Dictionary<string, ITypeMetadata> types;

    public TypeMetadataProvider(ITypeMetadataProvider parent)
    {
        this.parent = parent;
        types = new Dictionary<string, ITypeMetadata>();
    }

    public ITypeMetadata? GetType(string name)
        => types.GetValueOrDefault(name) ??
           parent.GetType(name);

    public bool DefineType(ITypeMetadata type)
        => type is TypeArgumentMetadata
            ? types.TryAdd(type.Name, type)
            : parent.DefineType(type);

    public ITypeMetadataProvider CreateChild()
        => new TypeMetadataProvider(this);
}