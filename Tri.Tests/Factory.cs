using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Tri.Tests;

internal static class Factory
{
    public static ArrayMetadata CreateArrayMetadata(ITypeMetadata itemMetadata, RootNamespaceMetadata ns)
    {
        var metadata = new ArrayMetadata(null, itemMetadata)
        {
            Namespace = ns,
        };

        var i64 = ns.FindType("i64")!;
        var sizeField = new FieldMetadata(metadata, "<>_size", i64);
        var sizeProperty = CreatePropertyMetadata(ns, metadata, "size", i64);

        metadata.AddField(sizeField);
        metadata.AddProperty(sizeProperty);
        metadata.AddMethod(sizeProperty.Getter!);

        return metadata;
    }

    public static FunctionTypeMetadata CreateFunctionType(
        IEnumerable<ITypeMetadata> parameterTypes,
        ITypeMetadata returnType,
        RootNamespaceMetadata ns)
    {
        var @void = ns.FindType("void")!;
        var @null = ns.FindType("null")!;

        var metadata = new FunctionTypeMetadata(null, parameterTypes, returnType)
        {
            Namespace = ns,
        };
        metadata.AddField(
            new FieldMetadata(
                metadata,
                FunctionTypeMetadata.FunctionField,
                new TypePointerMetadata(@void)));
        metadata.AddField(
            new FieldMetadata(
                metadata,
                FunctionTypeMetadata.ContextField,
                new DiscriminatedUnionMetadata(
                    null,
                    [new InterfaceMetadata(null, [], []), @null])));

        return metadata;
    }

    public static TupleMetadata CreateTupleMetadata(IReadOnlyList<ITypeMetadata> types, RootNamespaceMetadata ns)
    {
        var metadata = new TupleMetadata(null)
        {
            Namespace = ns,
        };

        for (var i = 0; i < types.Count; i++)
        {
            var type = types[i];
            var itemField = new FieldMetadata(metadata, $"<>_{i}", type);
            var itemProperty = CreatePropertyMetadata(ns, metadata, i.ToString(), type);

            metadata.AddType(type);
            metadata.AddField(itemField);
            metadata.AddProperty(itemProperty);
        }

        return metadata;
    }

    public static PropertyMetadata CreatePropertyMetadata(
        RootNamespaceMetadata ns,
        ITypeMetadata declaringType,
        string name,
        ITypeMetadata type,
        AccessModifierMetadata? getterModifier = null,
        AccessModifierMetadata? setterModifier = null)
    {
        var hasGetter = getterModifier is not null;
        var hasSetter = setterModifier is not null;
        var getter = default(MethodMetadata?);
        var setter = default(MethodMetadata?);

        if (!hasGetter && !hasSetter)
        {
            getter = GenerateGetter(ns, declaringType, name, type, AccessModifierMetadata.Public);
            setter = GenerateSetter(ns, declaringType, name, type, AccessModifierMetadata.Private);
        }
        else
        {
            if (hasGetter)
                getter = GenerateGetter(ns, declaringType, name, type, getterModifier!.Value);

            if (hasSetter)
                setter = GenerateSetter(ns, declaringType, name, type, setterModifier!.Value);
        }

        var metadata = new PropertyMetadata(
            null,
            declaringType,
            name,
            type,
            getter,
            setter);

        return metadata;
    }

    private static MethodMetadata GenerateGetter(
        RootNamespaceMetadata ns,
        ITypeMetadata declaringType,
        string name,
        ITypeMetadata type,
        AccessModifierMetadata getterModifier)
    {
        var functionType = CreateFunctionType([], type, ns);

        return new MethodMetadata(
            null,
            declaringType,
            getterModifier,
            false,
            $"<>_get_{name}",
            [],
            functionType);
    }

    private static MethodMetadata GenerateSetter(
        RootNamespaceMetadata ns,
        ITypeMetadata declaringType,
        string name,
        ITypeMetadata type,
        AccessModifierMetadata setterModifier)
    {
        var @void = ns.FindType("void")!;
        var functionType = CreateFunctionType([type], @void, ns);

        return new MethodMetadata(
            null,
            declaringType,
            setterModifier,
            false,
            $"<>_set_{name}",
            [new ParameterMetadata(null, MemberAccessExpression.Value, type)],
            functionType);
    }
}