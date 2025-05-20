using Trilang.Metadata;

namespace Trilang.Semantics.MetadataGenerators;

public class TypeArgumentMap
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
        typeProvider.DefineType(metadata.ToString(), metadata);

        return metadata;
    }

    private FunctionTypeMetadata Map(FunctionTypeMetadata functionType)
    {
        var parameterTypes = functionType.ParameterTypes.Select(Map).ToList();
        var returnType = Map(functionType.ReturnType);
        var functionTypeMetadata = new FunctionTypeMetadata(parameterTypes, returnType);
        typeProvider.DefineType(functionTypeMetadata.ToString(), functionTypeMetadata);

        return functionTypeMetadata;
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
            .Select(x => new InterfaceMethodMetadata(interfaceMetadata, x.Name, Map(x.TypeMetadata)));

        var metadata = new InterfaceMetadata(properties, methods);

        typeProvider.DefineType(metadata.ToString(), metadata);

        return metadata;
    }

    private TupleMetadata Map(TupleMetadata tuple)
    {
        var types = tuple.Types.Select(Map);
        var tupleMetadata = new TupleMetadata(types);
        typeProvider.DefineType(tupleMetadata.ToString(), tupleMetadata);

        return tupleMetadata;
    }

    private ITypeMetadata Map(TypeAliasMetadata type)
        => new TypeAliasMetadata(type.Name, type.GenericArguments.Select(Map), Map(type.Type!));

    private ITypeMetadata Map(TypeArgumentMetadata type)
        => map.GetValueOrDefault(type.Name, type);

    private TypeArrayMetadata Map(TypeArrayMetadata type)
    {
        var itemType = Map(type.ItemMetadata!);
        var typeArrayMetadata = new TypeArrayMetadata(itemType);
        typeProvider.DefineType(typeArrayMetadata.ToString(), typeArrayMetadata);

        return typeArrayMetadata;
    }

    private TypeMetadata Map(TypeMetadata type)
    {
        var closed = new TypeMetadata(
            type.Name,
            type.GenericArguments.Select(Map).ToList(),
            type.Interfaces.Select(Map).ToList(),
            [],
            [],
            []);

        foreach (var property in type.Properties)
            closed.AddProperty(
                new PropertyMetadata(
                    closed,
                    property.Name,
                    Map(property.Type),
                    property.GetterModifier,
                    property.SetterModifier));

        foreach (var constructor in type.Constructors)
            closed.AddConstructor(
                new ConstructorMetadata(
                    closed,
                    constructor.AccessModifier,
                    constructor.ParameterTypes.Select(Map).ToList()));

        foreach (var method in type.Methods)
            closed.AddMethod(
                new MethodMetadata(
                    closed,
                    method.AccessModifier,
                    method.Name,
                    Map(method.TypeMetadata)));

        typeProvider.DefineType(closed.ToString(), closed);

        return closed;
    }

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
                   interfaceMetadata.Methods.Select(x => x.TypeMetadata).Any(HasTypeArgument),

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
                   typeMetadata.Constructors.SelectMany(x => x.ParameterTypes).Any(HasTypeArgument) ||
                   typeMetadata.Methods.Select(x => x.TypeMetadata).Any(HasTypeArgument),

            _ => throw new ArgumentOutOfRangeException(nameof(type)),
        };
}