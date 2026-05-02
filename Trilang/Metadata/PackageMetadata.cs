namespace Trilang.Metadata;

public class PackageMetadata
{
    private readonly HashSet<PackageMetadata> dependencies;

    public PackageMetadata(string name, NamespaceMetadata @namespace)
    {
        dependencies = [];
        Name = name;
        Namespace = @namespace;
    }

    public void AddDependency(PackageMetadata dependency)
        => dependencies.Add(dependency);

    public FindNamespaceResult FindNamespace(IEnumerable<string> parts)
    {
        var current = Namespace;

        foreach (var part in parts)
        {
            current = current.GetNamespace(part);
            if (current is null)
                return FindNamespaceResult.NamespaceNotFound();
        }

        return FindNamespaceResult.Success(current);
    }

    public string Name { get; }

    public NamespaceMetadata Namespace { get; }

    public IReadOnlyCollection<PackageMetadata> Dependencies
        => dependencies;
}