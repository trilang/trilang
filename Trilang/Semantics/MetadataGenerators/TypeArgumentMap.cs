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

            result.map.Add(open.Name, specific);
        }

        return result;
    }

    public ITypeMetadata Map(ITypeMetadata type)
    {
        if (HasTypeArgument(type))
        {
            var mappedType = type switch
            {
                DiscriminatedUnionMetadata discriminatedUnionMetadata
                    => Map(discriminatedUnionMetadata),

                FunctionTypeMetadata functionTypeMetadata
                    => Map(functionTypeMetadata),

                InterfaceMetadata interfaceMetadata
                    => Map(interfaceMetadata),

                TupleMetadata tupleMetadata
                    => Map(tupleMetadata),

                // TODO: implement when generic aliases are supported
                TypeAliasMetadata typeAliasMetadata
                    => throw new NotImplementedException(),

                TypeArgumentMetadata typeArgumentMetadata
                    => Map(typeArgumentMetadata),

                TypeArrayMetadata typeArrayMetadata
                    => Map(typeArrayMetadata),

                // TODO: implement when generic methods are supported
                TypeMetadata typeMetadata
                    => throw new NotImplementedException(),

                _ => throw new ArgumentOutOfRangeException(nameof(type))
            };

            typeProvider.DefineType(mappedType);

            return mappedType;
        }

        return type;
    }

    private DiscriminatedUnionMetadata Map(DiscriminatedUnionMetadata discriminatedUnion)
    {
        var types = new ITypeMetadata[discriminatedUnion.Types.Count];
        for (var i = 0; i < discriminatedUnion.Types.Count; i++)
            types[i] = Map(discriminatedUnion.Types[i]);

        var name = string.Join(" | ", types.Select(x => x.Name));

        return new DiscriminatedUnionMetadata(name, types);
    }

    private FunctionTypeMetadata Map(FunctionTypeMetadata functionType)
    {
        var parameterTypes = functionType.ParameterTypes.Select(Map).ToList();
        var returnType = Map(functionType.ReturnType);

        var parameterNames = string.Join(", ", parameterTypes.Select(p => p.Name));
        var functionTypeName = $"({parameterNames}) => {returnType.Name}";
        var functionTypeMetadata = new FunctionTypeMetadata(functionTypeName);

        foreach (var parameterType in parameterTypes)
            functionTypeMetadata.AddParameter(parameterType);

        functionTypeMetadata.ReturnType = returnType;

        return functionTypeMetadata;
    }

    private InterfaceMetadata Map(InterfaceMetadata interfaceMetadata)
    {
        // TODO: remove?
        var propertyNames = interfaceMetadata.Properties.Select(f => $"{f.Name}: {Map(f.Type)};");
        var methodNames = interfaceMetadata.Methods
            .Select(m => $"{m.Name}({string.Join(", ", m.TypeMetadata.ParameterTypes.Select(Map))}): {Map(m.TypeMetadata.ReturnType)};");

        var combinedSignatures = propertyNames.Concat(methodNames).ToList();
        var name = combinedSignatures.Any()
            ? $"{{ {string.Join(" ", combinedSignatures)} }}"
            : "{ }";

        var result = new InterfaceMetadata(name);

        foreach (var property in interfaceMetadata.Properties)
            result.AddProperty(new InterfacePropertyMetadata(result, property.Name, Map(property.Type)));

        foreach (var method in interfaceMetadata.Methods)
            result.AddMethod(new InterfaceMethodMetadata(result, method.Name, Map(method.TypeMetadata)));

        return result;
    }

    private TupleMetadata Map(TupleMetadata tuple)
    {
        var types = new ITypeMetadata[tuple.Types.Count];
        for (var i = 0; i < tuple.Types.Count; i++)
            types[i] = Map(tuple.Types[i]);

        var name = $"({string.Join(", ", types.Select(x => x.Name))})";

        return new TupleMetadata(name, types);
    }

    private ITypeMetadata Map(TypeArgumentMetadata type)
        => map.GetValueOrDefault(type.Name, type);

    private TypeArrayMetadata Map(TypeArrayMetadata type)
    {
        var itemType = Map(type.ItemMetadata!);

        return new TypeArrayMetadata($"{itemType.Name}[]")
        {
            ItemMetadata = itemType
        };
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
                => typeMetadata.Interfaces.Any(HasTypeArgument) ||
                   typeMetadata.Properties.Select(x => x.Type).Any(HasTypeArgument) ||
                   typeMetadata.Constructors.SelectMany(x => x.ParameterTypes).Any(HasTypeArgument) ||
                   typeMetadata.Methods.Select(x => x.TypeMetadata).Any(HasTypeArgument),

            _ => throw new ArgumentOutOfRangeException(nameof(type)),
        };
}