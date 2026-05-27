using Trilang.Metadata;

namespace Trilang.Semantics.Providers;

public class MetadataProvider : IMetadataProvider
{
    private readonly CompilationContext compilationContext;
    private readonly INamespaceMetadata @namespace;
    private readonly HashSet<NamespaceMetadata> uses;

    public MetadataProvider(CompilationContext compilationContext, INamespaceMetadata @namespace)
    {
        this.compilationContext = compilationContext;
        this.@namespace = @namespace;
        uses = [];
    }

    public void AddUse(NamespaceMetadata use)
        => uses.Add(use);

    public QueryTypesResult QueryTypes(Query query)
        => query switch
        {
            GetArray getArray => GetArray(getArray),
            GetFunctionType getFunctionType => GetFunctionType(getFunctionType),
            GetGenericApplication getGenericApplication => GetGenericApplication(getGenericApplication),
            GetInterface getInterface => GetInterface(getInterface),
            GetOpenGeneric getOpenGeneric => GetOpenGeneric(getOpenGeneric),
            GetPointer getPointer => GetPointer(getPointer),
            ByName byName => ByName(byName),
            ByQualifiedName byQualifiedName => ByQualifiedName(byQualifiedName),
            GetTuple getTuple => GetTuple(getTuple),
            GetUnion getUnion => GetUnion(getUnion),

            _ => throw new ArgumentOutOfRangeException(nameof(query))
        };

    private static QueryTypesResult Result(IReadOnlyList<ITypeMetadata> found)
        => found.Count switch
        {
            0 => QueryTypesResult.TypeNotFound(),
            1 => QueryTypesResult.Success(found),
            _ => QueryTypesResult.MultipleTypesFound(found)
        };

    private QueryTypesResult QueryNamedTypes(Func<INamedMetadata, bool> predicate)
    {
        var found = new HashSet<ITypeMetadata>();

        var types = @namespace.Types
            .OfType<INamedMetadata>()
            .Where(predicate);

        foreach (var type in types)
            found.Add(type);

        foreach (var use in uses)
        {
            types = use.Types
                .OfType<INamedMetadata>()
                .Where(predicate);

            foreach (var type in types)
                found.Add(type);
        }

        if (@namespace != compilationContext.RootNamespace)
        {
            types = compilationContext.RootNamespace
                .EnumerateAllTypes()
                .OfType<INamedMetadata>()
                .Where(predicate);

            foreach (var type in types)
                found.Add(type);
        }

        return Result(found.ToArray());
    }

    private QueryTypesResult GetArray(GetArray query)
    {
        var found = new List<ITypeMetadata>();

        foreach (var type in compilationContext.RootNamespace.EnumerateAllTypes())
        {
            if (type is not ArrayMetadata array)
                continue;

            if (array.ItemMetadata == query.ElementType)
                found.Add(array);
        }

        return Result(found);
    }

    private QueryTypesResult GetFunctionType(GetFunctionType query)
    {
        var found = new List<ITypeMetadata>();

        foreach (var type in compilationContext.RootNamespace.EnumerateAllTypes())
        {
            if (type is not FunctionTypeMetadata functionType)
                continue;

            if (functionType.ParameterTypes.SequenceEqual(query.Parameters) &&
                functionType.ReturnType == query.ReturnType)
                found.Add(functionType);
        }

        return Result(found);
    }

    private QueryTypesResult GetGenericApplication(GetGenericApplication query)
    {
        var found = new List<ITypeMetadata>();

        foreach (var type in compilationContext.RootNamespace.EnumerateAllTypes())
        {
            if (type is not GenericApplicationMetadata generic)
                continue;

            if (generic.OpenGeneric == query.OpenGeneric &&
                generic.Arguments.SequenceEqual(query.TypeArguments))
                found.Add(generic);
        }

        return Result(found);
    }

    private QueryTypesResult GetInterface(GetInterface query)
    {
        var found = new List<ITypeMetadata>();

        foreach (var type in compilationContext.RootNamespace.EnumerateAllTypes())
        {
            if (type is not InterfaceMetadata @interface)
                continue;

            if (MatchesProperties(@interface.Properties, query.Properties) &&
                MatchesMethods(@interface.Methods, query.Methods))
                found.Add(@interface);
        }

        return Result(found);
    }

    private static bool MatchesProperties(
        IReadOnlyList<InterfacePropertyMetadata> properties,
        IReadOnlyList<GetInterfaceProperty> queryProperties)
    {
        if (properties.Count != queryProperties.Count)
            return false;

        for (var i = 0; i < properties.Count; i++)
            if (properties[i].Name != queryProperties[i].Name ||
                properties[i].Type != queryProperties[i].Type)
                return false;

        return true;
    }

    private static bool MatchesMethods(
        IReadOnlyList<InterfaceMethodMetadata> methods,
        IReadOnlyList<GetInterfaceMethod> queryMethods)
    {
        if (methods.Count != queryMethods.Count)
            return false;

        for (var i = 0; i < methods.Count; i++)
            if (methods[i].Name != queryMethods[i].Name ||
                !methods[i].Type.ParameterTypes.SequenceEqual(queryMethods[i].ParameterTypes) ||
                methods[i].Type.ReturnType != queryMethods[i].ReturnType)
                return false;

        return true;
    }

    private QueryTypesResult GetOpenGeneric(GetOpenGeneric query)
    {
        var result = QueryNamedTypes(x => x.Name == query.Name &&
                                          !x.IsCompilerGenerated &&
                                          x is IGenericMetadata { IsGeneric: true });

        if (query.TypeArgumentsCount is null ||
            result is { IsSuccess: false, IsMultipleTypesFound: false })
            return result;

        var found = new List<ITypeMetadata>();
        foreach (var type in result.Types.OfType<IGenericMetadata>())
            if (type.GenericArguments.Count == query.TypeArgumentsCount)
                found.Add(type);

        return Result(found);
    }

    private QueryTypesResult GetPointer(GetPointer query)
    {
        var found = new List<ITypeMetadata>();

        foreach (var type in compilationContext.RootNamespace.EnumerateAllTypes())
        {
            if (type is not PointerMetadata pointer)
                continue;

            if (pointer.Type == query.BaseType)
                found.Add(pointer);
        }

        return Result(found);
    }

    private QueryTypesResult ByName(ByName query)
        => QueryNamedTypes(x => x.Name == query.Name &&
                                !x.IsCompilerGenerated &&
                                x is not IGenericMetadata { IsGeneric: true });

    private QueryTypesResult ByQualifiedName(ByQualifiedName query)
    {
        var parts = query.Parts;
        var namespaceResult = compilationContext.FindNamespace(query.Package, parts.Take(parts.Count - 1));
        if (!namespaceResult.IsSuccess)
        {
            if (namespaceResult.IsPackageNotFound)
                return QueryTypesResult.PackageNotFound();

            return QueryTypesResult.NamespaceNotFound();
        }

        var name = parts[^1];
        var type = namespaceResult.Namespace.Types.OfType<INamedMetadata>().FirstOrDefault(x => x.Name == name);

        return type is null
            ? QueryTypesResult.TypeNotFound()
            : QueryTypesResult.Success([type]);
    }

    private QueryTypesResult GetTuple(GetTuple query)
    {
        var found = new List<ITypeMetadata>();

        foreach (var type in compilationContext.RootNamespace.EnumerateAllTypes())
        {
            if (type is not TupleMetadata tuple)
                continue;

            if (tuple.Types.SequenceEqual(query.Types))
                found.Add(tuple);
        }

        return Result(found);
    }

    private QueryTypesResult GetUnion(GetUnion query)
    {
        var found = new List<ITypeMetadata>();

        foreach (var type in compilationContext.RootNamespace.EnumerateAllTypes())
        {
            if (type is not DiscriminatedUnionMetadata union)
                continue;

            if (union.Types.SequenceEqual(query.Types))
                found.Add(union);
        }

        return Result(found);
    }

    public void DefineType(ITypeMetadata type)
    {
        if (type is IAnonymousTypeMetadata)
            compilationContext.RootNamespace.AddType(type);
        else
            @namespace.AddType(type);
    }

    public IReadOnlyList<FunctionMetadata> FindFunctions(string name)
    {
        var found = @namespace.FindFunction(name).ToList();

        foreach (var use in uses)
        foreach (var function in use.FindFunction(name))
            found.Add(function);

        return found.ToList();
    }

    public void AddFunction(FunctionMetadata function)
        => @namespace.AddFunction(function);
}