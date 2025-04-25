namespace Trilang.Metadata;

public class TypeMetadataProvider
{
    private readonly Dictionary<string, ITypeMetadata> types;

    public TypeMetadataProvider()
    {
        types = new Dictionary<string, ITypeMetadata>();

        DefineType(TypeMetadata.Void);

        DefineType(TypeMetadata.I8);
        DefineType(TypeMetadata.I16);
        DefineType(TypeMetadata.I32);
        DefineType(TypeMetadata.I64);

        DefineType(TypeMetadata.U8);
        DefineType(TypeMetadata.U16);
        DefineType(TypeMetadata.U32);
        DefineType(TypeMetadata.U64);

        DefineType(TypeMetadata.F32);
        DefineType(TypeMetadata.F64);

        DefineType(TypeMetadata.Bool);
        DefineType(TypeMetadata.Char);
        DefineType(TypeMetadata.String);
    }

    public ITypeMetadata? GetType(string name)
        => types.GetValueOrDefault(name);

    public bool DefineType(ITypeMetadata type)
        => types.TryAdd(type.Name, type);
}