namespace Trilang.Metadata;

public class BuiltInTypes
{
    public BuiltInTypes()
    {
        Void = new TypeMetadata(null, "void", [], [], [], [], [], [], true, false);
        VoidPointer = new PointerMetadata(null, Void);
        Null = new TypeMetadata(null, "null", [], [], [], [], [], [], true, false);
        I8 = new TypeMetadata(null, "i8", [], [], [], [], [], [], true, false);
        I16 = new TypeMetadata(null, "i16", [], [], [], [], [], [], true, false);
        I32 = new TypeMetadata(null, "i32", [], [], [], [], [], [], true, false);
        I64 = new TypeMetadata(null, "i64", [], [], [], [], [], [], true, false);
        U8 = new TypeMetadata(null, "u8", [], [], [], [], [], [], true, false);
        U16 = new TypeMetadata(null, "u16", [], [], [], [], [], [], true, false);
        U32 = new TypeMetadata(null, "u32", [], [], [], [], [], [], true, false);
        U64 = new TypeMetadata(null, "u64", [], [], [], [], [], [], true, false);
        F32 = new TypeMetadata(null, "f32", [], [], [], [], [], [], true, false);
        F64 = new TypeMetadata(null, "f64", [], [], [], [], [], [], true, false);
        Char = new TypeMetadata(null, "char", [], [], [], [], [], [], true, false);
        Bool = new TypeMetadata(null, "bool", [], [], [], [], [], [], true, false);
        String = new TypeMetadata(null, "string", [], [], [], [], [], [], false, false);
    }

    public void Freeze()
    {
        Void.Freeze();
        VoidPointer.Freeze();
        Null.Freeze();
        I8.Freeze();
        I16.Freeze();
        I32.Freeze();
        I64.Freeze();
        U8.Freeze();
        U16.Freeze();
        U32.Freeze();
        U64.Freeze();
        F32.Freeze();
        F64.Freeze();
        Char.Freeze();
        Bool.Freeze();
        String.Freeze();
    }

    public TypeMetadata Void { get; }

    public PointerMetadata VoidPointer { get; }

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