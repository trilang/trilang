using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Semantics.Providers;

namespace Trilang.Semantics.Passes;

internal class MetadataFactory
{
    private readonly BuiltInTypes builtInTypes;
    private readonly IMetadataProvider provider;

    public MetadataFactory(BuiltInTypes builtInTypes, IMetadataProvider provider)
    {
        this.builtInTypes = builtInTypes;
        this.provider = provider;
    }

    public ArrayMetadata CreateArrayMetadata(SourceLocation? definition, ITypeMetadata itemMetadata)
    {
        var metadata = new ArrayMetadata(definition, itemMetadata);
        var sizeField = new FieldMetadata(metadata, "<>_size", builtInTypes.I64);
        var sizeProperty = CreatePropertyMetadata(
            null,
            metadata,
            "size",
            builtInTypes.I64,
            AccessModifierMetadata.Public);

        metadata.AddField(sizeField);
        metadata.AddProperty(sizeProperty);
        metadata.AddMethod(sizeProperty.Getter!);

        return provider.GetOrDefine(metadata);
    }

    public FunctionTypeMetadata CreateFunctionType(
        SourceLocation? definition,
        IEnumerable<ITypeMetadata> parameterTypes,
        ITypeMetadata returnType)
    {
        // TODO: Move to BuildInTypes?
        var emptyInterface = new InterfaceMetadata(null, [], []);
        emptyInterface = provider.GetOrDefine(emptyInterface);

        var nullableEmptyInterface = new DiscriminatedUnionMetadata(null, [
            emptyInterface,
            builtInTypes.Null
        ]);
        nullableEmptyInterface = provider.GetOrDefine(nullableEmptyInterface);

        var metadata = new FunctionTypeMetadata(definition, parameterTypes, returnType);
        metadata.AddField(
            new FieldMetadata(
                metadata,
                FunctionTypeMetadata.FunctionField,
                new TypePointerMetadata(builtInTypes.Void))); // TODO: register in provider?
        metadata.AddField(
            new FieldMetadata(
                metadata,
                FunctionTypeMetadata.ContextField,
                nullableEmptyInterface));

        return provider.GetOrDefine(metadata);
    }

    public TupleMetadata CreateTupleMetadata(
        SourceLocation? definition,
        IEnumerable<ITypeMetadata> types)
    {
        var metadata = new TupleMetadata(definition);

        var typeItems = types as ITypeMetadata[] ?? types.ToArray();
        for (var i = 0; i < typeItems.Length; i++)
        {
            var type = typeItems[i];
            var itemField = new FieldMetadata(metadata, $"<>_{i}", type);
            var itemProperty = CreatePropertyMetadata(
                null,
                metadata,
                i.ToString(),
                type,
                AccessModifierMetadata.Public);

            metadata.AddType(type);
            metadata.AddField(itemField);
            metadata.AddProperty(itemProperty);
        }

        return provider.GetOrDefine(metadata);
    }

    public PropertyMetadata CreatePropertyMetadata(
        SourceLocation? definition,
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
            definition,
            declaringType,
            name,
            type,
            getter,
            setter);

        return metadata;
    }

    private MethodMetadata GenerateGetter(
        ITypeMetadata declaringType,
        string name,
        ITypeMetadata type,
        AccessModifierMetadata getterModifier)
    {
        var functionType = CreateFunctionType(null, [], type);

        return new MethodMetadata(
            null,
            declaringType,
            getterModifier,
            false,
            $"<>_get_{name}",
            [],
            functionType);
    }

    private MethodMetadata GenerateSetter(
        ITypeMetadata declaringType,
        string name,
        ITypeMetadata type,
        AccessModifierMetadata setterModifier)
    {
        var functionType = CreateFunctionType(null, [type], builtInTypes.Void);

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