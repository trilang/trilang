namespace Trilang.Metadata;

public class MetadataProvider : IMetadataProvider
{
    private readonly IMetadataProvider parent;
    private readonly Dictionary<string, ITypeMetadata> types;

    public MetadataProvider(IMetadataProvider parent)
    {
        this.parent = parent;
        types = new Dictionary<string, ITypeMetadata>();
    }

    public ITypeMetadata? GetType(string name)
        => types.GetValueOrDefault(name) ??
           parent.GetType(name);

    public bool DefineType(ITypeMetadata type)
        => DefineType(type.ToString()!, type);

    public bool DefineType(string name, ITypeMetadata type)
        => type is TypeArgumentMetadata
            ? types.TryAdd(name, type)
            : parent.DefineType(name, type);

    public T GetOrDefine<T>(T type) where T : ITypeMetadata
        => GetType(type.ToString()!) switch
        {
            null => DefineType(type.ToString()!, type) ? type : throw new InvalidOperationException(),
            T existingType => existingType,
            _ => throw new InvalidOperationException(),
        };

    public void AddFunction(FunctionMetadata function)
        => parent.AddFunction(function);

    public IMetadataProvider CreateChild()
        => new MetadataProvider(this);

    public IEnumerable<ITypeMetadata> Types
        => types.Values;

    public IReadOnlyList<FunctionMetadata> Functions
        => parent.Functions;
}