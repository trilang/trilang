using Trilang.Metadata;

namespace Trilang.Semantics.Providers;

public class FileMetadataProvider : IMetadataProvider
{
    private readonly HashSet<NamespaceMetadata> uses;

    public FileMetadataProvider(NamespaceMetadata @namespace)
    {
        Namespace = @namespace;
        uses = [];
    }

    public void AddUse(NamespaceMetadata use)
        => uses.Add(use);

    public QueryTypesResult QueryTypes(Query query)
        => query switch
        {
            ByName byName => ByName(byName),
            ByQualifiedName byQualifiedName => ByQualifiedName(byQualifiedName),

            _ => throw new ArgumentOutOfRangeException(nameof(query))
        };

    private QueryTypesResult ByName(ByName query)
    {
        var found = new List<ITypeMetadata>();

        var type = Namespace.FindType(query.Name);
        var typeExistsInPrimaryNamespace = type is not null;
        if (typeExistsInPrimaryNamespace)
            found.Add(type!);

        foreach (var use in uses)
        {
            type = use.FindType(query.Name);
            if (type is not null)
                found.Add(type);
        }

        if (!typeExistsInPrimaryNamespace)
        {
            for (var ns = Namespace.Parent; ns is not null; ns = ns.Parent)
            {
                type = ns.FindType(query.Name);
                if (type is not null)
                {
                    found.Add(type);
                    break;
                }
            }
        }

        return found.Count switch
        {
            0 => QueryTypesResult.TypeNotFound(),
            1 => QueryTypesResult.Success(found),
            _ => QueryTypesResult.MultipleTypesFound(found)
        };
    }

    private QueryTypesResult ByQualifiedName(ByQualifiedName query)
    {
        var parts = query.Parts;
        var ns = Namespace.FindNamespace(parts.Take(parts.Count - 1));
        if (ns is null)
            return QueryTypesResult.NamespaceNotFound();

        var type = ns.FindType(parts[^1]);

        return type is null
            ? QueryTypesResult.TypeNotFound()
            : QueryTypesResult.Success([type]);
    }

    public bool DefineType(string name, ITypeMetadata type)
        => Namespace.AddType(name, type);

    public IReadOnlyList<FunctionMetadata> FindFunctions(string name)
    {
        var found = new List<FunctionMetadata>();

        foreach (var function in Namespace.FindFunction(name))
            found.Add(function);

        var functionExistsInPrimaryNamespace = found.Count > 0;

        foreach (var use in uses)
        foreach (var function in use.FindFunction(name))
            found.Add(function);

        if (!functionExistsInPrimaryNamespace)
            for (var ns = Namespace.Parent; ns is not null; ns = ns.Parent)
                foreach (var function in ns.FindFunction(name))
                    found.Add(function);

        return found.ToList();
    }

    public void AddFunction(FunctionMetadata function)
        => Namespace.AddFunction(function);

    public NamespaceMetadata Namespace { get; }
}