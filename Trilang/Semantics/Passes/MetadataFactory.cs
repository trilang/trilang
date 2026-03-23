using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Semantics.Providers;

namespace Trilang.Semantics.Passes;

internal class MetadataFactory
{
    private readonly BuiltInTypes builtInTypes;
    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly IMetadataProvider provider;

    public MetadataFactory(
        BuiltInTypes builtInTypes,
        SemanticDiagnosticReporter diagnostics,
        IMetadataProvider provider)
    {
        this.builtInTypes = builtInTypes;
        this.diagnostics = diagnostics;
        this.provider = provider;
    }

    public ITypeMetadata Create(IInlineType inlineType)
        => inlineType switch
        {
            ArrayType arrayType => CreateArrayMetadata(
                arrayType.GetLocation(),
                arrayType.ElementType.Metadata ?? throw new InvalidOperationException()),

            DiscriminatedUnion discriminatedUnion => CreateDiscriminatedUnion(discriminatedUnion),

            FunctionType functionType => CreateFunctionType(
                functionType.GetLocation(),
                functionType.ParameterTypes.Select(t => t.Metadata ?? throw new InvalidOperationException()),
                functionType.ReturnType.Metadata ?? throw new InvalidOperationException()),

            GenericApplication genericApplication => CreateGenericApplication(genericApplication),

            Interface @interface => CreateInterface(@interface),

            TupleType tupleType => CreateTupleMetadata(
                tupleType.GetLocation(),
                tupleType.Types.Select(t => t.Metadata ?? throw new InvalidOperationException())),

            TypeRef typeRef => CreateTypeRef(typeRef),

            _ => throw new ArgumentOutOfRangeException(nameof(inlineType)),
        };

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

    private DiscriminatedUnionMetadata CreateDiscriminatedUnion(DiscriminatedUnion discriminatedUnion)
    {
        var metadata = new DiscriminatedUnionMetadata(
            discriminatedUnion.GetLocation(),
            discriminatedUnion.Types.Select(t => t.Metadata ?? throw new InvalidOperationException()));

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

    public GenericApplicationMetadata CreateGenericApplication(GenericApplication genericApplication)
    {
        var openGenericName = genericApplication.GetOpenGenericName();
        var openGeneric = default(IGenericMetadata);

        var result = provider.QueryTypes(Query.From(openGenericName));
        if (result is { IsSuccess: true, Types: [IGenericMetadata genericMetadata] })
        {
            openGeneric = genericMetadata;
        }
        else
        {
            if (result.IsMultipleTypesFound)
                diagnostics.MultipleMembersFound(genericApplication, result.Types);
            else if (result.IsNamespaceNotFound)
                diagnostics.UnknownNamespace(genericApplication);
            else
                diagnostics.UnknownType(genericApplication);

            openGeneric = TypeMetadata.Invalid(openGenericName);
        }

        var metadata = new GenericApplicationMetadata(
            genericApplication.GetLocation(),
            openGeneric,
            genericApplication.TypeArguments.Select(x => x.Metadata ??
                                                         throw new InvalidOperationException()));

        metadata = provider.GetOrDefine(metadata);

        return metadata;
    }

    private InterfaceMetadata CreateInterface(Interface @interface)
    {
        var metadata = new InterfaceMetadata(@interface.GetLocation());

        foreach (var property in @interface.Properties)
        {
            var propertyMetadata = new InterfacePropertyMetadata(
                metadata.Definition! with { Span = property.SourceSpan.GetValueOrDefault() },
                metadata,
                property.Name,
                property.Type.Metadata ?? throw new InvalidOperationException(),
                property.GetterModifier?.ToMetadata() ?? AccessModifierMetadata.Public,
                property.SetterModifier?.ToMetadata());

            if (!metadata.GetProperties(property.Name).IsEmpty)
            {
                propertyMetadata.MarkAsInvalid();
                diagnostics.InterfacePropertyAlreadyDefined(property);
            }

            metadata.AddProperty(propertyMetadata);
            property.Metadata = propertyMetadata;
        }

        foreach (var methods in @interface.Methods.GroupBy(x => x.Name))
        {
            foreach (var method in methods)
            {
                var parameters = new ITypeMetadata[method.ParameterTypes.Count];
                for (var i = 0; i < method.ParameterTypes.Count; i++)
                    parameters[i] = method.ParameterTypes[i].Metadata ??
                                    throw new InvalidOperationException();

                var functionType = CreateFunctionType(
                    null,
                    parameters,
                    method.ReturnType.Metadata ?? throw new InvalidOperationException());

                // TODO: generic?
                var methodMetadata = new InterfaceMethodMetadata(
                    metadata.Definition! with { Span = method.SourceSpan.GetValueOrDefault() },
                    metadata,
                    method.Name,
                    functionType);

                if (metadata.GetMethods(method.Name).MatchFunction(parameters).Any())
                {
                    methodMetadata.MarkAsInvalid();
                    diagnostics.InterfaceMethodAlreadyDefined(method);
                }

                metadata.AddMethod(methodMetadata);
                method.Metadata = methodMetadata;
            }
        }

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

    private ITypeMetadata CreateTypeRef(TypeRef typeRef)
    {
        diagnostics.UnknownType(typeRef);

        return TypeMetadata.Invalid(typeRef.Name);
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