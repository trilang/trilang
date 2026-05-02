namespace Trilang.Metadata;

public class BuiltInTypes
{
    public BuiltInTypes()
    {
        Void = new TypeMetadata(null, "void", [], [], [], [], [], [], true, true);
        Null = new TypeMetadata(null, "null", [], [], [], [], [], [], true, true);
        I8 = new TypeMetadata(null, "i8", [], [], [], [], [], [], true, true);
        I16 = new TypeMetadata(null, "i16", [], [], [], [], [], [], true, true);
        I32 = new TypeMetadata(null, "i32", [], [], [], [], [], [], true, true);
        I64 = new TypeMetadata(null, "i64", [], [], [], [], [], [], true, true);
        U8 = new TypeMetadata(null, "u8", [], [], [], [], [], [], true, true);
        U16 = new TypeMetadata(null, "u16", [], [], [], [], [], [], true, true);
        U32 = new TypeMetadata(null, "u32", [], [], [], [], [], [], true, true);
        U64 = new TypeMetadata(null, "u64", [], [], [], [], [], [], true, true);
        F32 = new TypeMetadata(null, "f32", [], [], [], [], [], [], true, true);
        F64 = new TypeMetadata(null, "f64", [], [], [], [], [], [], true, true);
        Char = new TypeMetadata(null, "char", [], [], [], [], [], [], true, true);
        Bool = new TypeMetadata(null, "bool", [], [], [], [], [], [], true, true);
        String = new TypeMetadata(null, "string", [], [], [], [], [], [], false, true);
    }

    public TypeMetadata Void { get; }

    public TypeMetadata Null { get; }

    public TypeMetadata I8 { get; }

    public TypeMetadata I16 { get; }

    public TypeMetadata I32 { get; }

    public TypeMetadata I64 { get; }

    public TypeMetadata U8 { get; }

    public TypeMetadata U16 { get; }

    public TypeMetadata U32 { get; }

    public TypeMetadata U64 { get; }

    public TypeMetadata F32 { get; }

    public TypeMetadata F64 { get; }

    public TypeMetadata Char { get; }

    public TypeMetadata Bool { get; }

    public TypeMetadata String { get; }
}