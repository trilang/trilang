using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Tri.Tests;

internal static class Factory
{
    public static ArrayMetadata CreateArrayMetadata(
        ITypeMetadata itemMetadata,
        NamespaceMetadata ns)
    {
        var metadata = new ArrayMetadata(null, itemMetadata)
        {
            Namespace = ns,
        };

        var i64 = new TypeMetadata(null, "i64", [], [], [], [], [], [], true)
        {
            Namespace = ns,
        };
        var sizeField = new FieldMetadata(metadata, "<>_size", i64);
        var sizeProperty = CreatePropertyMetadata(metadata, "size", i64);

        metadata.AddField(sizeField);
        metadata.AddProperty(sizeProperty);
        metadata.AddMethod(sizeProperty.Getter!);

        return metadata;
    }

    public static FunctionTypeMetadata CreateFunctionType(
        IEnumerable<ITypeMetadata> parameterTypes,
        ITypeMetadata returnType,
        NamespaceMetadata ns)
    {
        var metadata = new FunctionTypeMetadata(null, parameterTypes, returnType)
        {
            Namespace = ns,
        };
        metadata.AddField(
            new FieldMetadata(
                metadata,
                FunctionTypeMetadata.FunctionField,
                new TypePointerMetadata(
                    new TypeMetadata(null, "void", [], [], [], [], [], [], true)
                    {
                        Namespace = ns,
                    })));
        metadata.AddField(
            new FieldMetadata(
                metadata,
                FunctionTypeMetadata.ContextField,
                new DiscriminatedUnionMetadata(null, [
                    new InterfaceMetadata(null, [], []),
                    new TypeMetadata(null, "null", [], [], [], [], [], [], true)
                    {
                        Namespace = ns,
                    }
                ])));

        return metadata;
    }

    public static TupleMetadata CreateTupleMetadata(
        IReadOnlyList<ITypeMetadata> types,
        NamespaceMetadata ns)
    {
        var metadata = new TupleMetadata(null)
        {
            Namespace = ns,
        };

        for (var i = 0; i < types.Count; i++)
        {
            var type = types[i];
            var itemField = new FieldMetadata(metadata, $"<>_{i}", type);
            var itemProperty = CreatePropertyMetadata(metadata, i.ToString(), type);

            metadata.AddType(type);
            metadata.AddField(itemField);
            metadata.AddProperty(itemProperty);
        }

        return metadata;
    }

    public static PropertyMetadata CreatePropertyMetadata(
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
            getter = GenerateGetter(declaringType, name, type, AccessModifierMetadata.Public);
            setter = GenerateSetter(declaringType, name, type, AccessModifierMetadata.Private);
        }
        else
        {
            if (hasGetter)
                getter = GenerateGetter(declaringType, name, type, getterModifier!.Value);

            if (hasSetter)
                setter = GenerateSetter(declaringType, name, type, setterModifier!.Value);
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
        ITypeMetadata declaringType,
        string name,
        ITypeMetadata type,
        AccessModifierMetadata getterModifier)
    {
        var functionType = CreateFunctionType([], type, GetRoot(declaringType.Namespace!));

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
        ITypeMetadata declaringType,
        string name,
        ITypeMetadata type,
        AccessModifierMetadata setterModifier)
    {
        var ns = GetRoot(declaringType.Namespace!);
        var functionType = CreateFunctionType([type],
            new TypeMetadata(null, "void", [], [], [], [], [], [], true)
            {
                Namespace = ns,
            },
            ns);

        return new MethodMetadata(
            null,
            declaringType,
            setterModifier,
            false,
            $"<>_set_{name}",
            [new ParameterMetadata(null, MemberAccessExpression.Value, type)],
            functionType);
    }

    private static NamespaceMetadata GetRoot(NamespaceMetadata ns)
    {
        while (true)
        {
            if (ns.Parent is null)
                return ns;

            ns = ns.Parent;
        }
    }
}