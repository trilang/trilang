using Trilang.Metadata;

namespace Trilang.Semantics.Passes.MetadataGenerators;

internal class TypeArgumentMap
{
    private readonly ITypeMetadataProvider typeProvider;
    private readonly Dictionary<string, ITypeMetadata> map;

    private TypeArgumentMap(ITypeMetadataProvider typeProvider)
    {
        this.typeProvider = typeProvider;
        map = [];
    }

    public static TypeArgumentMap Create(
        ITypeMetadataProvider typeProvider,
        IReadOnlyCollection<ITypeMetadata> closedTypes,
        IReadOnlyCollection<ITypeMetadata> openTypes)
    {
        var result = new TypeArgumentMap(typeProvider);

        foreach (var (specific, open) in closedTypes.Zip(openTypes))
        {
            if (specific is TypeArgumentMetadata || specific.Equals(open))
                continue;

            result.map.Add(open.ToString()!, specific);
        }

        return result;
    }

    public ITypeMetadata Map(ITypeMetadata type)
    {
        if (!HasTypeArgument(type))
            return type;

        return type switch
        {
            DiscriminatedUnionMetadata discriminatedUnionMetadata
                => Map(discriminatedUnionMetadata),

            FunctionTypeMetadata functionTypeMetadata
                => Map(functionTypeMetadata),

            InterfaceMetadata interfaceMetadata
                => Map(interfaceMetadata),

            TupleMetadata tupleMetadata
                => Map(tupleMetadata),

            TypeAliasMetadata typeAliasMetadata
                => Map(typeAliasMetadata),

            TypeArgumentMetadata typeArgumentMetadata
                => Map(typeArgumentMetadata),

            TypeArrayMetadata typeArrayMetadata
                => Map(typeArrayMetadata),

            TypeMetadata typeMetadata
                => Map(typeMetadata),

            _ => throw new ArgumentOutOfRangeException(nameof(type))
        };
    }

    private DiscriminatedUnionMetadata Map(DiscriminatedUnionMetadata discriminatedUnion)
    {
        var types = new ITypeMetadata[discriminatedUnion.Types.Count];
        for (var i = 0; i < discriminatedUnion.Types.Count; i++)
            types[i] = Map(discriminatedUnion.Types[i]);

        var metadata = new DiscriminatedUnionMetadata(types);

        return typeProvider.GetOrDefine(metadata);
    }

    private FunctionTypeMetadata Map(FunctionTypeMetadata functionType)
    {
        var parameterTypes = functionType.ParameterTypes.Select(Map).ToList();
        var returnType = Map(functionType.ReturnType);
        var functionTypeMetadata = new FunctionTypeMetadata(parameterTypes, returnType);

        return typeProvider.GetOrDefine(functionTypeMetadata);
    }

    private InterfaceMetadata Map(InterfaceMetadata interfaceMetadata)
    {
        var properties = interfaceMetadata.Properties
            .Select(x => new InterfacePropertyMetadata(
                interfaceMetadata,
                x.Name,
                Map(x.Type),
                x.GetterModifier,
                x.SetterModifier));

        var methods = interfaceMetadata.Methods
            .Select(x => new InterfaceMethodMetadata(interfaceMetadata, x.Name, Map(x.Type)));

        var metadata = new InterfaceMetadata(properties, methods);

        return typeProvider.GetOrDefine(metadata);
    }

    private TupleMetadata Map(TupleMetadata tuple)
    {
        var types = tuple.Types.Select(Map);
        var tupleMetadata = new TupleMetadata(types);

        return typeProvider.GetOrDefine(tupleMetadata);
    }

    private ITypeMetadata Map(TypeAliasMetadata type)
    {
        var alias = new TypeAliasMetadata(type.Name, type.GenericArguments.Select(Map), Map(type.Type!));

        return typeProvider.GetOrDefine(alias);
    }

    private ITypeMetadata Map(TypeArgumentMetadata type)
        => map.GetValueOrDefault(type.Name, type);

    private TypeArrayMetadata Map(TypeArrayMetadata type)
    {
        var itemType = Map(type.ItemMetadata!);
        var typeArrayMetadata = new TypeArrayMetadata(itemType);

        return typeProvider.GetOrDefine(typeArrayMetadata);
    }

    private TypeMetadata Map(TypeMetadata type)
    {
        var closed = new TypeMetadata(
            type.Name,
            type.GenericArguments.Select(Map).ToList(),
            [],
            [],
            [],
            [],
            []);

        if (typeProvider.GetType(closed.ToString()) is TypeMetadata existingType)
            return existingType;

        typeProvider.DefineType(closed.ToString(), closed);

        foreach (var @interface in type.Interfaces)
            closed.AddInterface(Map(@interface));

        foreach (var property in type.Properties)
        {
            var getter = default(MethodMetadata);
            if (property.Getter is not null)
                getter = Map(closed, property.Getter);

            var setter = default(MethodMetadata);
            if (property.Setter is not null)
                setter = Map(closed, property.Setter);

            closed.AddProperty(
                new PropertyMetadata(
                    closed,
                    property.Name,
                    Map(property.Type),
                    getter,
                    setter));
        }

        foreach (var constructor in type.Constructors)
            closed.AddConstructor(
                new ConstructorMetadata(
                    closed,
                    constructor.AccessModifier,
                    constructor.Parameters.Select(Map).ToList(),
                    Map(constructor.Type)));

        foreach (var method in type.Methods)
            closed.AddMethod(Map(closed, method));

        return closed;
    }

    private ParameterMetadata Map(ParameterMetadata parameter)
        => new ParameterMetadata(parameter.Name, Map(parameter.Type));

    private MethodMetadata Map(TypeMetadata closed, MethodMetadata method)
        => new MethodMetadata(
            closed,
            method.AccessModifier,
            method.IsStatic,
            method.Name,
            method.Parameters.Select(Map).ToList(),
            Map(method.Type));

    private bool HasTypeArgument(ITypeMetadata type)
        => type switch
        {
            DiscriminatedUnionMetadata discriminatedUnionMetadata
                => discriminatedUnionMetadata.Types.Any(HasTypeArgument),

            FunctionTypeMetadata functionTypeMetadata
                => functionTypeMetadata.ParameterTypes.Any(HasTypeArgument) ||
                   HasTypeArgument(functionTypeMetadata.ReturnType),

            InterfaceMetadata interfaceMetadata
                => interfaceMetadata.Properties.Select(x => x.Type).Any(HasTypeArgument) ||
                   interfaceMetadata.Methods.Select(x => x.Type).Any(HasTypeArgument),

            TupleMetadata tupleMetadata
                => tupleMetadata.Types.Any(HasTypeArgument),

            TypeAliasMetadata typeAliasMetadata
                => HasTypeArgument(typeAliasMetadata.Type!),

            TypeArgumentMetadata
                => true,

            TypeArrayMetadata typeArrayMetadata
                => HasTypeArgument(typeArrayMetadata.ItemMetadata!),

            TypeMetadata typeMetadata
                => typeMetadata.GenericArguments.Any(HasTypeArgument) ||
                   typeMetadata.Interfaces.Any(HasTypeArgument) ||
                   typeMetadata.Properties.Select(x => x.Type).Any(HasTypeArgument) ||
                   typeMetadata.Constructors.SelectMany(x => x.Parameters).Select(x => x.Type).Any(HasTypeArgument) ||
                   typeMetadata.Methods.Select(x => x.Type).Any(HasTypeArgument),

            _ => throw new ArgumentOutOfRangeException(nameof(type)),
        };
}