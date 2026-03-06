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

    public IReadOnlyList<ITypeMetadata> FindTypes(string name)
    {
        var found = new List<ITypeMetadata>();

        var type = Namespace.FindType(name);
        var typeExistsInPrimaryNamespace = type is not null;
        if (typeExistsInPrimaryNamespace)
            found.Add(type!);

        foreach (var use in uses)
        {
            type = use.FindType(name);
            if (type is not null)
                found.Add(type);
        }

        if (!typeExistsInPrimaryNamespace)
        {
            for (var ns = Namespace.Parent; ns is not null; ns = ns.Parent)
            {
                type = ns.FindType(name);
                if (type is not null)
                {
                    found.Add(type);
                    break;
                }
            }
        }

        return found;
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