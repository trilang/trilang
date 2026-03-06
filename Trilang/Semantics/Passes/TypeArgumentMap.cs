using Trilang.Metadata;
using Trilang.Semantics.Providers;

namespace Trilang.Semantics.Passes;

internal class TypeArgumentMap
{
    private readonly BuiltInTypes builtInTypes;
    private readonly GenericApplicationMetadata genericApplication;
    private readonly Dictionary<ITypeMetadata, ITypeMetadata> map;

    public TypeArgumentMap(BuiltInTypes builtInTypes, GenericApplicationMetadata genericApplication)
    {
        this.builtInTypes = builtInTypes;
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
        var metadata = new AliasMetadata(
            type.Definition,
            type.Name,
            type.GenericArguments.Select(Map),
            Map(type.Type!));

        if (type.IsInvalid)
            metadata.MarkAsInvalid();

        // TODO: query?
        var @namespace = type.Namespace!;
        if (@namespace.FindType(metadata.Name) is not null)
            throw new InvalidOperationException($"The '{metadata.Name}' alias is already defined.");

        @namespace.AddType(metadata.ToString(), metadata);

        return metadata;
    }

    private ArrayMetadata Map(ArrayMetadata arrayMetadata)
    {
        var provider = new FileMetadataProvider(arrayMetadata.Namespace!);
        var metadataFactory = new MetadataFactory(builtInTypes, provider);

        var itemType = Map(arrayMetadata.ItemMetadata!);
        var metadata = metadataFactory.CreateArrayMetadata(arrayMetadata.Definition, itemType);

        if (arrayMetadata.IsInvalid)
            metadata.MarkAsInvalid();

        return metadata;
    }

    private DiscriminatedUnionMetadata Map(DiscriminatedUnionMetadata discriminatedUnion)
    {
        var provider = new FileMetadataProvider(discriminatedUnion.Namespace!);

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
        var provider = new FileMetadataProvider(functionType.Namespace!);
        var metadataFactory = new MetadataFactory(builtInTypes, provider);

        var parameterTypes = functionType.ParameterTypes.Select(Map).ToList();
        var returnType = Map(functionType.ReturnType);
        var metadata = metadataFactory.CreateFunctionType(
            functionType.Definition,
            parameterTypes,
            returnType);

        if (functionType.IsInvalid)
            metadata.MarkAsInvalid();

        return metadata;
    }

    private GenericApplicationMetadata Map(GenericApplicationMetadata genericApplicationMetadata)
    {
        var provider = new FileMetadataProvider(genericApplicationMetadata.Namespace!);

        var metadata = new GenericApplicationMetadata(
            genericApplicationMetadata.Definition,
            genericApplicationMetadata.OpenGeneric,
            genericApplicationMetadata.Arguments.Select(Map).ToList());

        metadata = provider.GetOrDefine(metadata);

        var nestedMap = new TypeArgumentMap(builtInTypes, metadata);
        nestedMap.Map();

        return metadata;
    }

    private InterfaceMetadata Map(InterfaceMetadata interfaceMetadata)
    {
        var provider = new FileMetadataProvider(interfaceMetadata.Namespace!);

        var properties = interfaceMetadata.Properties
            .Select(x => new InterfacePropertyMetadata(
                x.Definition,
                interfaceMetadata,
                x.Name,
                Map(x.Type),
                x.GetterModifier,
                x.SetterModifier));

        var methods = interfaceMetadata.Methods
            .Select(x => new InterfaceMethodMetadata(
                x.Definition,
                interfaceMetadata,
                x.Name,
                Map(x.Type)));

        var metadata = new InterfaceMetadata(interfaceMetadata.Definition, properties, methods);

        if (interfaceMetadata.IsInvalid)
            metadata.MarkAsInvalid();

        return provider.GetOrDefine(metadata);
    }

    private TupleMetadata Map(TupleMetadata tuple)
    {
        var provider = new FileMetadataProvider(tuple.Namespace!);
        var metadataFactory = new MetadataFactory(builtInTypes, provider);

        var types = tuple.Types.Select(Map);
        var metadata = metadataFactory.CreateTupleMetadata(tuple.Definition, types);

        if (tuple.IsInvalid)
            metadata.MarkAsInvalid();

        return metadata;
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

        // TODO: query?
        var @namespace = type.Namespace!;
        if (@namespace.FindType(metadata.ToString()) is not null)
            throw new InvalidOperationException($"The '{metadata.Name}' type is already defined.");

        @namespace.AddType(metadata.ToString(), metadata);

        foreach (var @interface in type.Interfaces)
            metadata.AddInterface(Map(@interface));

        foreach (var property in type.Properties)
        {
            var getter = default(MethodMetadata);
            if (property.Getter is not null)
                getter = Map(metadata, property.Getter);

            var setter = default(MethodMetadata);
            if (property.Setter is not null)
                setter = Map(metadata, property.Setter);

            metadata.AddProperty(
                new PropertyMetadata(
                    null,
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
            foreach (var method in methods)
                metadata.AddMethod(Map(metadata, method));
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

    private MethodMetadata Map(TypeMetadata closed, MethodMetadata method)
    {
        var metadata = new MethodMetadata(
            method.Definition,
            closed,
            method.AccessModifier,
            method.IsStatic,
            method.Name,
            method.Parameters.Select(Map).ToList(),
            Map(method.Type));

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