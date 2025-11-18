using Trilang.Metadata;

namespace Trilang.IntermediateRepresentation;

// TODO: use alignment/pack
internal class TypeLayoutGenerator
{
    // TODO: use platform pointer size
    private const int PointerSize = 8;

    public void Generate(IEnumerable<ITypeMetadata> types)
    {
        GenerateBuiltInTypes();

        foreach (var type in types)
        {
            if (type.Layout is not null)
                continue;

            type.Layout = Generate(type);
        }
    }

    private void GenerateBuiltInTypes()
    {
        TypeMetadata.Void.Layout = new TypeLayout();

        TypeMetadata.Null.Layout = new TypeLayout([
            new FieldLayout(null, 0, PointerSize),
        ]);

        TypeMetadata.I8.Layout = new TypeLayout([
            new FieldLayout(null, 0, 1),
        ]);

        TypeMetadata.I16.Layout = new TypeLayout([
            new FieldLayout(null, 0, 2),
        ]);

        TypeMetadata.I32.Layout = new TypeLayout([
            new FieldLayout(null, 0, 4),
        ]);

        TypeMetadata.I64.Layout = new TypeLayout([
            new FieldLayout(null, 0, 8),
        ]);

        TypeMetadata.U8.Layout = new TypeLayout([
            new FieldLayout(null, 0, 1),
        ]);

        TypeMetadata.U16.Layout = new TypeLayout([
            new FieldLayout(null, 0, 2),
        ]);

        TypeMetadata.U32.Layout = new TypeLayout([
            new FieldLayout(null, 0, 4),
        ]);

        TypeMetadata.U64.Layout = new TypeLayout([
            new FieldLayout(null, 0, 8),
        ]);

        TypeMetadata.F32.Layout = new TypeLayout([
            new FieldLayout(null, 0, 4),
        ]);

        TypeMetadata.F64.Layout = new TypeLayout([
            new FieldLayout(null, 0, 8),
        ]);

        TypeMetadata.Bool.Layout = new TypeLayout([
            new FieldLayout(null, 0, 1),
        ]);

        TypeMetadata.Char.Layout = new TypeLayout([
            new FieldLayout(null, 0, 2),
        ]);
    }

    private TypeLayout? Generate(ITypeMetadata type)
        => type switch
        {
            AliasMetadata aliasMetadata
                => Generate(aliasMetadata),

            DiscriminatedUnionMetadata discriminatedUnionMetadata
                => Generate(discriminatedUnionMetadata),

            FunctionTypeMetadata functionTypeMetadata
                => Generate(functionTypeMetadata),

            InterfaceMetadata interfaceMetadata
                => Generate(interfaceMetadata),

            TupleMetadata tupleMetadata
                => Generate(tupleMetadata),

            TypeArgumentMetadata typeArgumentMetadata
                => Generate(typeArgumentMetadata),

            ArrayMetadata typeArrayMetadata
                => Generate(typeArrayMetadata),

            TypeMetadata typeMetadata
                => Generate(typeMetadata),

            TypePointerMetadata typePointerMetadata
                => Generate(typePointerMetadata),

            _ => throw new ArgumentOutOfRangeException(nameof(type)),
        };

    private TypeLayout? Generate(AliasMetadata alias)
    {
        var type = alias.Type!;
        if (type.Layout is not null)
            return type.Layout;

        type.Layout = Generate(type);

        return type.Layout;
    }

    private TypeLayout Generate(DiscriminatedUnionMetadata du)
    {
        var layout = new TypeLayout();

        // TODO: use field?
        layout.AddGap(PointerSize);
        layout.AddGap(du.Types.Select(GetFieldSize).Max());

        return layout;
    }

    private TypeLayout Generate(FunctionTypeMetadata function)
    {
        var layout = new TypeLayout();
        foreach (var field in function.Fields)
            layout.AddField(field, GetFieldSize(field.Type));

        return layout;
    }

    private TypeLayout? Generate(InterfaceMetadata _)
        => null;

    private TypeLayout Generate(TupleMetadata tuple)
    {
        var layout = new TypeLayout();
        foreach (var field in tuple.Fields)
            layout.AddField(field, GetFieldSize(field.Type));

        return layout;
    }

    private TypeLayout Generate(TypeArgumentMetadata type)
        => throw new NotSupportedException($"Can't calculate a type layout for the type argument ({type}).");

    private TypeLayout Generate(ArrayMetadata type)
    {
        var layout = new TypeLayout();
        foreach (var field in type.Fields)
            layout.AddField(field, GetFieldSize(field.Type));

        return layout;
    }

    private TypeLayout Generate(TypeMetadata type)
    {
        var layout = new TypeLayout();
        foreach (var field in type.Fields)
            layout.AddField(field, GetFieldSize(field.Type));

        return layout;
    }

    private TypeLayout Generate(TypePointerMetadata _)
    {
        var layout = new TypeLayout();

        // pointer to actual type
        layout.AddGap(PointerSize);

        return layout;
    }

    private int GetFieldSize(ITypeMetadata fieldType)
    {
        var size = PointerSize;
        if (fieldType.IsValueType)
        {
            fieldType.Layout ??= Generate(fieldType);
            size = fieldType.Layout!.Size;
        }

        return size;
    }
}