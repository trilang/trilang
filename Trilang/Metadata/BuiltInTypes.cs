namespace Trilang.Metadata;

public class BuiltInTypes
{
    public BuiltInTypes()
    {
        Void = new TypeMetadata(null, AccessModifierMetadata.Public, "void", [], [], [], [], [], [], true, false);
        VoidPointer = new PointerMetadata(null, Void);
        Null = new TypeMetadata(null, AccessModifierMetadata.Public, "null", [], [], [], [], [], [], true, false);
        I8 = new TypeMetadata(null, AccessModifierMetadata.Public, "i8", [], [], [], [], [], [], true, false);
        I16 = new TypeMetadata(null, AccessModifierMetadata.Public, "i16", [], [], [], [], [], [], true, false);
        I32 = new TypeMetadata(null, AccessModifierMetadata.Public, "i32", [], [], [], [], [], [], true, false);
        I64 = new TypeMetadata(null, AccessModifierMetadata.Public, "i64", [], [], [], [], [], [], true, false);
        U8 = new TypeMetadata(null, AccessModifierMetadata.Public, "u8", [], [], [], [], [], [], true, false);
        U16 = new TypeMetadata(null, AccessModifierMetadata.Public, "u16", [], [], [], [], [], [], true, false);
        U32 = new TypeMetadata(null, AccessModifierMetadata.Public, "u32", [], [], [], [], [], [], true, false);
        U64 = new TypeMetadata(null, AccessModifierMetadata.Public, "u64", [], [], [], [], [], [], true, false);
        F32 = new TypeMetadata(null, AccessModifierMetadata.Public, "f32", [], [], [], [], [], [], true, false);
        F64 = new TypeMetadata(null, AccessModifierMetadata.Public, "f64", [], [], [], [], [], [], true, false);
        Char = new TypeMetadata(null, AccessModifierMetadata.Public, "char", [], [], [], [], [], [], true, false);
        Bool = new TypeMetadata(null, AccessModifierMetadata.Public, "bool", [], [], [], [], [], [], true, false);
        String = new TypeMetadata(null, AccessModifierMetadata.Public, "string", [], [], [], [], [], [], false, false);
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