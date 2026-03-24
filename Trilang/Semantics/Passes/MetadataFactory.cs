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
                arrayType.ElementType.Metadata.Required()),

            DiscriminatedUnion discriminatedUnion => CreateDiscriminatedUnion(
                discriminatedUnion.GetLocation(),
                discriminatedUnion.Types.Select(t => t.Metadata.Required()).ToArray()),

            FunctionType functionType => CreateFunctionType(
                functionType.GetLocation(),
                functionType.ParameterTypes.Select(t => t.Metadata.Required()).ToArray(),
                functionType.ReturnType.Metadata.Required()),

            GenericApplication genericApplication => CreateGenericApplication(
                genericApplication.GetLocation(),
                (IGenericMetadata)genericApplication.Type.Metadata.Required(),
                genericApplication.TypeArguments.Select(x => x.Metadata.Required()).ToArray()),

            Interface @interface => CreateInterface(@interface),

            TupleType tupleType => CreateTupleMetadata(
                tupleType.GetLocation(),
                tupleType.Types.Select(t => t.Metadata.Required()).ToArray()),

            TypeRef typeRef => CreateTypeRef(typeRef),

            _ => throw new ArgumentOutOfRangeException(nameof(inlineType)),
        };

    public ArrayMetadata CreateArrayMetadata(SourceLocation? definition, ITypeMetadata itemMetadata)
    {
        var result = provider.QueryTypes(new GetArray(itemMetadata));
        if (result.IsSuccess || result.IsMultipleTypesFound)
            return (ArrayMetadata)result.Types[0];

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

        provider.DefineType(metadata);

        return metadata;
    }

    public DiscriminatedUnionMetadata CreateDiscriminatedUnion(
        SourceLocation? definition,
        IReadOnlyList<ITypeMetadata> types)
    {
        var result = provider.QueryTypes(new GetUnion(types));
        if (result.IsSuccess || result.IsMultipleTypesFound)
            return (DiscriminatedUnionMetadata)result.Types[0];

        var metadata = new DiscriminatedUnionMetadata(definition, types);

        provider.DefineType(metadata);

        return metadata;
    }

    public FunctionTypeMetadata CreateFunctionType(
        SourceLocation? definition,
        IReadOnlyList<ITypeMetadata> parameterTypes,
        ITypeMetadata returnType)
    {
        // TODO: Move to BuildInTypes?
        var emptyInterface = default(InterfaceMetadata);
        var result = provider.QueryTypes(new GetInterface([], []));
        if (result.IsSuccess || result.IsMultipleTypesFound)
        {
            emptyInterface = (InterfaceMetadata)result.Types[0];
        }
        else
        {
            emptyInterface = new InterfaceMetadata(null, [], []);
            provider.DefineType(emptyInterface);
        }

        var nullableEmptyInterface = default(DiscriminatedUnionMetadata);
        result = provider.QueryTypes(new GetUnion([emptyInterface, builtInTypes.Null]));
        if (result.IsSuccess || result.IsMultipleTypesFound)
        {
            nullableEmptyInterface = (DiscriminatedUnionMetadata)result.Types[0];
        }
        else
        {
            nullableEmptyInterface = new DiscriminatedUnionMetadata(null, [
                emptyInterface,
                builtInTypes.Null
            ]);
            provider.DefineType(nullableEmptyInterface);
        }

        result = provider.QueryTypes(new GetFunctionType(parameterTypes, returnType));
        if (result.IsSuccess || result.IsMultipleTypesFound)
            return (FunctionTypeMetadata)result.Types[0];

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

        provider.DefineType(metadata);

        return metadata;
    }

    public GenericApplicationMetadata CreateGenericApplication(
        SourceLocation? definition,
        IGenericMetadata openGeneric,
        IReadOnlyList<ITypeMetadata> arguments)
    {
        var getGenericApplication = new GetGenericApplication(openGeneric, arguments);
        var result = provider.QueryTypes(getGenericApplication);
        if (result.IsSuccess || result.IsMultipleTypesFound)
            return (GenericApplicationMetadata)result.Types[0];

        var metadata = new GenericApplicationMetadata(definition, openGeneric, arguments);

        provider.DefineType(metadata);

        return metadata;
    }

    private InterfaceMetadata CreateInterface(Interface @interface)
    {
        var byInterface = new GetInterface(
            @interface.Properties.Select(x => new GetInterfaceProperty(
                x.Name,
                x.Type.Metadata.Required())).ToArray(),
            @interface.Methods.Select(x => new GetInterfaceMethod(
                x.Name,
                x.ParameterTypes.Select(t => t.Metadata.Required()).ToArray(),
                x.ReturnType.Metadata.Required())).ToArray());
        var result = provider.QueryTypes(byInterface);
        if (result.IsSuccess || result.IsMultipleTypesFound)
            return (InterfaceMetadata)result.Types[0];

        var metadata = new InterfaceMetadata(@interface.GetLocation());

        foreach (var property in @interface.Properties)
        {
            var propertyMetadata = new InterfacePropertyMetadata(
                metadata.Definition! with { Span = property.SourceSpan.GetValueOrDefault() },
                metadata,
                property.Name,
                property.Type.Metadata.Required(),
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

        foreach (var method in @interface.Methods)
        {
            var parameters = new ITypeMetadata[method.ParameterTypes.Count];
            for (var i = 0; i < method.ParameterTypes.Count; i++)
                parameters[i] = method.ParameterTypes[i].Metadata.Required();

            var functionType = CreateFunctionType(
                null,
                parameters,
                method.ReturnType.Metadata.Required());

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

        provider.DefineType(metadata);

        return metadata;
    }

    public InterfaceMetadata CreateInterface(InterfaceMetadata metadata)
    {
        var byInterface = new GetInterface(
            metadata.Properties
                .Select(x => new GetInterfaceProperty(x.Name, x.Type))
                .ToArray(),
            metadata.Methods
                .Select(x => new GetInterfaceMethod(x.Name, x.Type.ParameterTypes, x.Type.ReturnType))
                .ToArray());
        var result = provider.QueryTypes(byInterface);
        if (result.IsSuccess || result.IsMultipleTypesFound)
            return (InterfaceMetadata)result.Types[0];

        provider.DefineType(metadata);

        return metadata;
    }

    public TupleMetadata CreateTupleMetadata(
        SourceLocation? definition,
        IReadOnlyList<ITypeMetadata> types)
    {
        var result = provider.QueryTypes(new GetTuple(types));
        if (result.IsSuccess || result.IsMultipleTypesFound)
            return (TupleMetadata)result.Types[0];

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

        provider.DefineType(metadata);

        return metadata;
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