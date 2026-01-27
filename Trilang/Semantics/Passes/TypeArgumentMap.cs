using Trilang.Metadata;

namespace Trilang.Semantics.Passes;

internal class TypeArgumentMap
{
    private readonly IMetadataProvider provider;
    private readonly GenericApplicationMetadata genericApplication;
    private readonly Dictionary<ITypeMetadata, ITypeMetadata> map;

    public TypeArgumentMap(
        IMetadataProvider provider,
        GenericApplicationMetadata genericApplication)
    {
        this.provider = provider;
        this.genericApplication = genericApplication;
        this.map = genericApplication.OpenGeneric.GenericArguments
            .Zip(genericApplication.Arguments)
            .ToDictionary();
    }

    public void Map()
    {
        // skip mapping if it is just the same generic with different type parameters
        if (genericApplication.Arguments.All(x => x is TypeArgumentMetadata))
            return;

        genericApplication.ClosedGeneric = Map(genericApplication.OpenGeneric);
    }

    private ITypeMetadata Map(ITypeMetadata type)
    {
        if (!HasTypeArgument(type))
            return type;

        return type switch
        {
            AliasMetadata aliasMetadata
                => Map(aliasMetadata),

            ArrayMetadata typeArrayMetadata
                => Map(typeArrayMetadata),

            DiscriminatedUnionMetadata discriminatedUnionMetadata
                => Map(discriminatedUnionMetadata),

            FunctionTypeMetadata functionTypeMetadata
                => Map(functionTypeMetadata),

            GenericApplicationMetadata genericApplicationMetadata
                => Map(genericApplicationMetadata),

            InterfaceMetadata interfaceMetadata
                => Map(interfaceMetadata),

            TupleMetadata tupleMetadata
                => Map(tupleMetadata),

            TypeArgumentMetadata typeArgumentMetadata
                => Map(typeArgumentMetadata),

            TypeMetadata typeMetadata
                => Map(typeMetadata),

            _ => throw new ArgumentOutOfRangeException(nameof(type))
        };
    }

    private AliasMetadata Map(AliasMetadata type)
    {
        var alias = new AliasMetadata(
            type.Definition,
            type.Name,
            type.GenericArguments.Select(Map),
            Map(type.Type!));

        if (type.IsInvalid)
            alias.MarkAsInvalid();

        return provider.GetOrDefine(alias);
    }

    private ArrayMetadata Map(ArrayMetadata arrayMetadata)
    {
        var itemType = Map(arrayMetadata.ItemMetadata!);
        var metadata = new ArrayMetadata(arrayMetadata.Definition, itemType);

        if (arrayMetadata.IsInvalid)
            metadata.MarkAsInvalid();

        return provider.GetOrDefine(metadata);
    }

    private DiscriminatedUnionMetadata Map(DiscriminatedUnionMetadata discriminatedUnion)
    {
        var types = new ITypeMetadata[discriminatedUnion.Types.Count];
        for (var i = 0; i < discriminatedUnion.Types.Count; i++)
            types[i] = Map(discriminatedUnion.Types[i]);

        var metadata = new DiscriminatedUnionMetadata(discriminatedUnion.Definition, types);

        if (discriminatedUnion.IsInvalid)
            metadata.MarkAsInvalid();

        return provider.GetOrDefine(metadata);
    }

    private FunctionTypeMetadata Map(FunctionTypeMetadata functionType)
    {
        var parameterTypes = functionType.ParameterTypes.Select(Map).ToList();
        var returnType = Map(functionType.ReturnType);
        var metadata = new FunctionTypeMetadata(functionType.Definition, parameterTypes, returnType);

        if (functionType.IsInvalid)
            metadata.MarkAsInvalid();

        return provider.GetOrDefine(metadata);
    }

    private GenericApplicationMetadata Map(GenericApplicationMetadata genericApplicationMetadata)
    {
        var metadata = provider.GetOrDefine(
            new GenericApplicationMetadata(
                genericApplicationMetadata.Definition,
                genericApplicationMetadata.OpenGeneric,
                genericApplicationMetadata.Arguments.Select(Map).ToList()));

        var nestedMap = new TypeArgumentMap(provider, metadata);
        nestedMap.Map();

        return metadata;
    }

    private InterfaceMetadata Map(InterfaceMetadata interfaceMetadata)
    {
        var properties = interfaceMetadata.Properties
            .Select(x => new InterfacePropertyMetadata(
                x.Definition,
                interfaceMetadata,
                x.Name,
                Map(x.Type),
                x.GetterModifier,
                x.SetterModifier));

        var methods = interfaceMetadata.Methods
            .GroupBy(method => method.Name)
            .Select(g => new { g, functionGroup = new FunctionGroupMetadata() })
            .SelectMany(t => t.g, (t, x) => new InterfaceMethodMetadata(
                x.Definition,
                interfaceMetadata,
                x.Name,
                Map(x.Type),
                t.functionGroup));

        var metadata = new InterfaceMetadata(interfaceMetadata.Definition, properties, methods);

        if (interfaceMetadata.IsInvalid)
            metadata.MarkAsInvalid();

        return provider.GetOrDefine(metadata);
    }

    private TupleMetadata Map(TupleMetadata tuple)
    {
        var types = tuple.Types.Select(Map);
        var metadata = new TupleMetadata(tuple.Definition, types);

        if (tuple.IsInvalid)
            metadata.MarkAsInvalid();

        return provider.GetOrDefine(metadata);
    }

    private ITypeMetadata Map(TypeArgumentMetadata type)
        => map.TryGetValue(type, out var mapped)
            ? mapped
            : throw new InvalidOperationException($"Type argument {type.Name} is not mapped.");

    private TypeMetadata Map(TypeMetadata type)
    {
        var metadata = new TypeMetadata(
            type.Definition,
            type.Name,
            type.GenericArguments.Select(Map).ToList(),
            [],
            [],
            [],
            [],
            []);

        if (provider.GetType(metadata.ToString()) is TypeMetadata existingType)
            return existingType;

        provider.DefineType(metadata.ToString(), metadata);

        foreach (var @interface in type.Interfaces)
            metadata.AddInterface(Map(@interface));

        foreach (var property in type.Properties)
        {
            var getter = default(MethodMetadata);
            if (property.Getter is not null)
                getter = Map(metadata, property.Getter, new FunctionGroupMetadata());

            var setter = default(MethodMetadata);
            if (property.Setter is not null)
                setter = Map(metadata, property.Setter, new FunctionGroupMetadata());

            metadata.AddProperty(
                new PropertyMetadata(
                    metadata,
                    property.Name,
                    Map(property.Type),
                    getter,
                    setter));
        }

        foreach (var constructor in type.Constructors)
            metadata.AddConstructor(
                new ConstructorMetadata(
                    constructor.Definition,
                    metadata,
                    constructor.AccessModifier,
                    constructor.Parameters.Select(Map).ToList(),
                    Map(constructor.Type)));

        foreach (var methods in type.Methods.GroupBy(x => x.Name))
        {
            var group = new FunctionGroupMetadata();

            foreach (var method in methods)
                metadata.AddMethod(Map(metadata, method, group));
        }

        if (type.IsInvalid)
            metadata.MarkAsInvalid();

        return metadata;
    }

    private ParameterMetadata Map(ParameterMetadata parameter)
    {
        var metadata = new ParameterMetadata(
            parameter.Definition,
            parameter.Name,
            Map(parameter.Type));

        if (parameter.IsInvalid)
            metadata.MarkAsInvalid();

        return metadata;
    }

    private MethodMetadata Map(TypeMetadata closed, MethodMetadata method, FunctionGroupMetadata group)
    {
        var metadata = new MethodMetadata(
            method.Definition,
            closed,
            method.AccessModifier,
            method.IsStatic,
            method.Name,
            method.Parameters.Select(Map).ToList(),
            Map(method.Type),
            group);

        if (method.IsInvalid)
            metadata.MarkAsInvalid();

        return metadata;
    }

    private bool HasTypeArgument(ITypeMetadata type)
        => type switch
        {
            AliasMetadata typeAliasMetadata
                => typeAliasMetadata.GenericArguments.Any(HasTypeArgument) ||
                   HasTypeArgument(typeAliasMetadata.Type!),

            ArrayMetadata typeArrayMetadata
                => HasTypeArgument(typeArrayMetadata.ItemMetadata!),

            DiscriminatedUnionMetadata discriminatedUnionMetadata
                => discriminatedUnionMetadata.Types.Any(HasTypeArgument),

            FunctionTypeMetadata functionTypeMetadata
                => functionTypeMetadata.ParameterTypes.Any(HasTypeArgument) ||
                   HasTypeArgument(functionTypeMetadata.ReturnType),

            GenericApplicationMetadata genericApplicationMetadata
                => genericApplicationMetadata.Arguments.Any(HasTypeArgument),

            InterfaceMetadata interfaceMetadata
                => interfaceMetadata.Properties.Select(x => x.Type).Any(HasTypeArgument) ||
                   interfaceMetadata.Methods.Select(x => x.Type).Any(HasTypeArgument),

            TupleMetadata tupleMetadata
                => tupleMetadata.Types.Any(HasTypeArgument),

            TypeArgumentMetadata
                => true,

            TypeMetadata typeMetadata
                => typeMetadata.GenericArguments.Any(HasTypeArgument) ||
                   typeMetadata.Interfaces.Any(HasTypeArgument) ||
                   typeMetadata.Properties.Select(x => x.Type).Any(HasTypeArgument) ||
                   typeMetadata.Constructors.SelectMany(x => x.Parameters).Select(x => x.Type).Any(HasTypeArgument) ||
                   typeMetadata.Methods.Select(x => x.Type).Any(HasTypeArgument),

            _ => throw new ArgumentOutOfRangeException(nameof(type)),
        };
}