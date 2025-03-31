namespace Trilang.Metadata;

public class TypeMetadataStore
{
    private readonly Dictionary<string, TypeMetadata> types;

    public TypeMetadataStore()
    {
        types = new Dictionary<string, TypeMetadata>
        {
            { "void", TypeMetadata.Void },

            { "i8", TypeMetadata.I8 },
            { "i16", TypeMetadata.I16 },
            { "i32", TypeMetadata.I32 },
            { "i64", TypeMetadata.I64 },

            { "u8", TypeMetadata.U8 },
            { "u16", TypeMetadata.U16 },
            { "u32", TypeMetadata.U32 },
            { "u64", TypeMetadata.U64 },

            { "f32", TypeMetadata.F32 },
            { "f64", TypeMetadata.F64 },

            { "bool", TypeMetadata.Bool },
            { "char", TypeMetadata.Char },
            { "string", TypeMetadata.String },
        };
    }

    public TypeMetadata? GetType(string name)
        => types.GetValueOrDefault(name);
}