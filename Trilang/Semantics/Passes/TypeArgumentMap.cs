using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Providers;

namespace Trilang.Semantics.Passes;

internal class TypeArgumentMap
{
    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly CompilationContext compilationContext;
    private readonly GenericApplicationMetadata genericApplication;
    private readonly Dictionary<ITypeMetadata, ITypeMetadata> map;
    private readonly MetadataProvider provider;
    private readonly MetadataFactory metadataFactory;

    public TypeArgumentMap(
        SemanticDiagnosticReporter diagnostics,
        CompilationContext compilationContext,
        GenericApplicationMetadata genericApplication)
    {
        this.diagnostics = diagnostics;
        this.compilationContext = compilationContext;
        this.genericApplication = genericApplication;
        this.provider = new MetadataProvider(compilationContext, compilationContext.RootNamespace);
        this.metadataFactory = new MetadataFactory(compilationContext.BuiltInTypes, diagnostics, provider);
        this.map = genericApplication.OpenGeneric.GenericArguments
            .Zip(genericApplication.Arguments)
            .ToDictionary();
    }

    public void Map()
        => Map([]);

    private void Map(Dictionary<ITypeMetadata, ITypeMetadata> visited)
    {
        // skip mapping if it is just the same generic with different type parameters
        if (genericApplication.Arguments.All(x => x is TypeArgumentMetadata))
            return;

        genericApplication.ClosedGeneric = Map(genericApplication.OpenGeneric, visited);
    }

    private ITypeMetadata Map(ITypeMetadata type, Dictionary<ITypeMetadata, ITypeMetadata> visited)
    {
        if (visited.TryGetValue(type, out var mapped))
            return mapped;

        if (!HasTypeArgument(type))
            return type;

        return type switch
        {
            AliasMetadata aliasMetadata
                => Map(aliasMetadata, visited),

            ArrayMetadata typeArrayMetadata
                => Map(typeArrayMetadata, visited),

            DiscriminatedUnionMetadata discriminatedUnionMetadata
                => Map(discriminatedUnionMetadata, visited),

            FunctionTypeMetadata functionTypeMetadata
                => Map(functionTypeMetadata, visited),

            GenericApplicationMetadata genericApplicationMetadata
                => Map(genericApplicationMetadata, visited),

            InterfaceMetadata interfaceMetadata
                => Map(interfaceMetadata, visited),

            PointerMetadata pointerMetadata
                => Map(pointerMetadata, visited),

            TupleMetadata tupleMetadata
                => Map(tupleMetadata, visited),

            TypeArgumentMetadata typeArgumentMetadata
                => Map(typeArgumentMetadata),

            TypeMetadata typeMetadata
                => Map(typeMetadata, visited),

            _ => throw new ArgumentOutOfRangeException(nameof(type))
        };
    }

    private AliasMetadata Map(AliasMetadata type, Dictionary<ITypeMetadata, ITypeMetadata> visited)
    {
        var metadata = new AliasMetadata(
            type.Definition,
            type.Name,
            [],
            null,
            true);
        visited[type] = metadata;

        foreach (var genericArgument in type.GenericArguments)
            metadata.AddGenericArgument(Map(genericArgument, visited));

        metadata.Type = Map(type.Type!, visited);

        if (type.IsInvalid)
            metadata.MarkAsInvalid();

        provider.DefineType(metadata);

        return metadata;
    }

    private ArrayMetadata Map(ArrayMetadata arrayMetadata, Dictionary<ITypeMetadata, ITypeMetadata> visited)
    {
        var itemType = Map(arrayMetadata.ItemMetadata!, visited);
        var metadata = metadataFactory.CreateArrayMetadata(arrayMetadata.Definition, itemType);
        visited[arrayMetadata] = metadata;

        if (arrayMetadata.IsInvalid)
            metadata.MarkAsInvalid();

        return metadata;
    }

    private DiscriminatedUnionMetadata Map(
        DiscriminatedUnionMetadata discriminatedUnion,
        Dictionary<ITypeMetadata, ITypeMetadata> visited)
    {
        var types = new ITypeMetadata[discriminatedUnion.Types.Count];
        for (var i = 0; i < discriminatedUnion.Types.Count; i++)
            types[i] = Map(discriminatedUnion.Types[i], visited);

        var metadata = metadataFactory.CreateDiscriminatedUnion(discriminatedUnion.Definition, types);
        visited[discriminatedUnion] = metadata;

        if (discriminatedUnion.IsInvalid)
            metadata.MarkAsInvalid();

        return metadata;
    }

    private FunctionTypeMetadata Map(FunctionTypeMetadata functionType, Dictionary<ITypeMetadata, ITypeMetadata> visited)
    {
        var parameterTypes = functionType.ParameterTypes.Select(x => Map(x, visited)).ToList();
        var returnType = Map(functionType.ReturnType, visited);
        var metadata = metadataFactory.CreateFunctionType(
            functionType.Definition,
            parameterTypes,
            returnType);
        visited[functionType] = metadata;

        if (functionType.IsInvalid)
            metadata.MarkAsInvalid();

        return metadata;
    }

    private GenericApplicationMetadata Map(
        GenericApplicationMetadata genericApplicationMetadata,
        Dictionary<ITypeMetadata, ITypeMetadata> visited)
    {
        var metadata = metadataFactory.CreateGenericApplication(
            genericApplicationMetadata.Definition,
            genericApplicationMetadata.OpenGeneric,
            genericApplicationMetadata.Arguments.Select(x => Map(x, visited)).ToList());
        visited[genericApplicationMetadata] = metadata;

        var nestedMap = new TypeArgumentMap(diagnostics, compilationContext, metadata);
        nestedMap.Map(visited);

        return metadata;
    }

    private InterfaceMetadata Map(InterfaceMetadata interfaceMetadata, Dictionary<ITypeMetadata, ITypeMetadata> visited)
    {
        var properties = interfaceMetadata.Properties
            .Select(x => new InterfacePropertyMetadata(
                x.Definition,
                interfaceMetadata,
                x.Name,
                Map(x.Type, visited),
                x.GetterModifier,
                x.SetterModifier));

        var methods = interfaceMetadata.Methods
            .Select(x => new InterfaceMethodMetadata(
                x.Definition,
                interfaceMetadata,
                x.Name,
                Map(x.Type, visited)));

        var metadata = new InterfaceMetadata(interfaceMetadata.Definition, properties, methods);
        metadata = metadataFactory.CreateInterface(metadata);
        visited[interfaceMetadata] = metadata;

        if (interfaceMetadata.IsInvalid)
            metadata.MarkAsInvalid();

        return metadata;
    }

    private PointerMetadata Map(PointerMetadata pointer, Dictionary<ITypeMetadata, ITypeMetadata> visited)
    {
        var metadata = metadataFactory.CreatePointer(pointer.Definition, Map(pointer.Type, visited));
        visited[pointer] = metadata;

        if (pointer.IsInvalid)
            metadata.MarkAsInvalid();

        return metadata;
    }

    private TupleMetadata Map(TupleMetadata tuple, Dictionary<ITypeMetadata, ITypeMetadata> visited)
    {
        var types = tuple.Types.Select(x => Map(x, visited)).ToArray();
        var metadata = metadataFactory.CreateTupleMetadata(tuple.Definition, types);
        visited[tuple] = metadata;

        if (tuple.IsInvalid)
            metadata.MarkAsInvalid();

        return metadata;
    }

    private ITypeMetadata Map(TypeArgumentMetadata type)
        => map.TryGetValue(type, out var mapped)
            ? mapped
            : throw new InvalidOperationException($"Type argument {type.Name} is not mapped.");

    private TypeMetadata Map(TypeMetadata type, Dictionary<ITypeMetadata, ITypeMetadata> visited)
    {
        var metadata = new TypeMetadata(
            type.Definition,
            type.Name,
            [],
            [],
            [],
            [],
            [],
            [],
            type.IsValueType,
            true);
        visited[type] = metadata;

        foreach (var genericArgument in type.GenericArguments)
            metadata.AddGenericArgument(Map(genericArgument, visited));

        // we aren't checking for duplicates because closed generic types
        // are generated based on generic applications.
        // generic applications are anonymous types and deduplicated automatically.
        provider.DefineType(metadata);

        foreach (var @interface in type.Interfaces)
            metadata.AddInterface(Map(@interface, visited));

        foreach (var property in type.Properties)
        {
            var getter = default(MethodMetadata);
            if (property.Getter is not null)
                getter = Map(metadata, property.Getter, visited);

            var setter = default(MethodMetadata);
            if (property.Setter is not null)
                setter = Map(metadata, property.Setter, visited);

            metadata.AddProperty(
                new PropertyMetadata(
                    null,
                    metadata,
                    property.Name,
                    Map(property.Type, visited),
                    getter,
                    setter));
        }

        foreach (var constructor in type.Constructors)
            metadata.AddConstructor(
                new ConstructorMetadata(
                    constructor.Definition,
                    metadata,
                    constructor.AccessModifier,
                    constructor.Parameters.Select(x => Map(x, visited)).ToList(),
                    Map(constructor.Type, visited)));

        foreach (var method in type.Methods)
            metadata.AddMethod(Map(metadata, method, visited));

        if (type.IsInvalid)
            metadata.MarkAsInvalid();

        return metadata;
    }

    private ParameterMetadata Map(ParameterMetadata parameter, Dictionary<ITypeMetadata, ITypeMetadata> visited)
    {
        var metadata = new ParameterMetadata(
            parameter.Definition,
            parameter.Name,
            Map(parameter.Type, visited));

        if (parameter.IsInvalid)
            metadata.MarkAsInvalid();

        return metadata;
    }

    private MethodMetadata Map(TypeMetadata closed, MethodMetadata method, Dictionary<ITypeMetadata, ITypeMetadata> visited)
    {
        var metadata = new MethodMetadata(
            method.Definition,
            closed,
            method.AccessModifier,
            method.IsStatic,
            method.Name,
            method.Parameters.Select(x => Map(x, visited)).ToList(),
            Map(method.Type, visited));

        if (method.IsInvalid)
            metadata.MarkAsInvalid();

        return metadata;
    }

    private bool HasTypeArgument(ITypeMetadata type)
        => HasTypeArgument(type, []);

    private bool HasTypeArgument(ITypeMetadata type, HashSet<ITypeMetadata> visited)
    {
        if (!visited.Add(type))
            return false;

        return type switch
        {
            AliasMetadata typeAliasMetadata
                => typeAliasMetadata.GenericArguments.Any(HasTypeArgument) ||
                   HasTypeArgument(typeAliasMetadata.Type!, visited),

            ArrayMetadata typeArrayMetadata
                => HasTypeArgument(typeArrayMetadata.ItemMetadata!, visited),

            DiscriminatedUnionMetadata discriminatedUnionMetadata
                => discriminatedUnionMetadata.Types.Any(t => HasTypeArgument(t, visited)),

            FunctionTypeMetadata functionTypeMetadata
                => functionTypeMetadata.ParameterTypes.Any(t => HasTypeArgument(t, visited)) ||
                   HasTypeArgument(functionTypeMetadata.ReturnType, visited),

            GenericApplicationMetadata genericApplicationMetadata
                => genericApplicationMetadata.Arguments.Any(t => HasTypeArgument(t, visited)),

            InterfaceMetadata interfaceMetadata
                => interfaceMetadata.Properties.Select(x => x.Type).Any(t => HasTypeArgument(t, visited)) ||
                   interfaceMetadata.Methods.Select(x => x.Type).Any(t => HasTypeArgument(t, visited)),

            PointerMetadata pointerMetadata
                => HasTypeArgument(pointerMetadata.Type, visited),

            TupleMetadata tupleMetadata
                => tupleMetadata.Types.Any(t => HasTypeArgument(t, visited)),

            TypeArgumentMetadata
                => true,

            TypeMetadata typeMetadata
                => typeMetadata.GenericArguments.Any(t => HasTypeArgument(t, visited)) ||
                   typeMetadata.Interfaces.Any(t => HasTypeArgument(t, visited)) ||
                   typeMetadata.Properties.Select(x => x.Type).Any(t => HasTypeArgument(t, visited)) ||
                   typeMetadata.Constructors.Select(x => x.Type).Any(t => HasTypeArgument(t, visited)) ||
                   typeMetadata.Methods.Select(x => x.Type).Any(t => HasTypeArgument(t, visited)) ||
                   typeMetadata.Fields.Select(x => x.Type).Any(t => HasTypeArgument(t, visited)),

            _ => throw new ArgumentOutOfRangeException(nameof(type)),
        };
    }
}