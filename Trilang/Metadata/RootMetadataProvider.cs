namespace Trilang.Metadata;

// TODO: is not thread safe
public class RootMetadataProvider : IMetadataProvider
{
    private readonly Dictionary<string, ITypeMetadata> types;
    private readonly List<FunctionMetadata> functions;

    public RootMetadataProvider()
    {
        types = [];
        functions = [];

        DefineType(TypeMetadata.Void.Name, TypeMetadata.Void);
        DefineType(TypeMetadata.Null.Name, TypeMetadata.Null);

        DefineType(TypeMetadata.I8.Name, TypeMetadata.I8);
        DefineType(TypeMetadata.I16.Name, TypeMetadata.I16);
        DefineType(TypeMetadata.I32.Name, TypeMetadata.I32);
        DefineType(TypeMetadata.I64.Name, TypeMetadata.I64);

        DefineType(TypeMetadata.U8.Name, TypeMetadata.U8);
        DefineType(TypeMetadata.U16.Name, TypeMetadata.U16);
        DefineType(TypeMetadata.U32.Name, TypeMetadata.U32);
        DefineType(TypeMetadata.U64.Name, TypeMetadata.U64);

        DefineType(TypeMetadata.F32.Name, TypeMetadata.F32);
        DefineType(TypeMetadata.F64.Name, TypeMetadata.F64);

        DefineType(TypeMetadata.Bool.Name, TypeMetadata.Bool);
        DefineType(TypeMetadata.Char.Name, TypeMetadata.Char);
        DefineType(TypeMetadata.String.Name, TypeMetadata.String);
    }

    public ITypeMetadata? GetType(string name)
        => types.GetValueOrDefault(name);

    public bool DefineType(ITypeMetadata type)
        => DefineType(type.ToString()!, type);

    public bool DefineType(string name, ITypeMetadata type)
        => types.TryAdd(name, type);

    public T GetOrDefine<T>(T type) where T : ITypeMetadata
        => GetType(type.ToString()!) switch
        {
            null => DefineType(type.ToString()!, type) ? type : throw new InvalidOperationException(),
            T existingType => existingType,
            _ => throw new InvalidOperationException(),
        };

    public void AddFunction(FunctionMetadata function)
        => functions.Add(function);

    public IMetadataProvider CreateChild()
        => new MetadataProvider(this);

    public IEnumerable<ITypeMetadata> Types
        => types.Values;

    public IReadOnlyList<FunctionMetadata> Functions
        => functions;
}