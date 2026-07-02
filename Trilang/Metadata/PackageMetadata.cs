namespace Trilang.Metadata;

public class PackageMetadata
{
    private readonly HashSet<PackageMetadata> dependencies;
    private bool isFrozen;

    public PackageMetadata(string name)
    {
        dependencies = [];
        Name = name;
        Namespace = NamespaceMetadata.CreateForPackage(this);
    }

    public void AddDependency(PackageMetadata dependency)
    {
        EnsureNotFrozen();
        dependencies.Add(dependency);
    }

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

    public void Freeze()
    {
        isFrozen = true;

        Namespace.Freeze();
    }

    private void EnsureNotFrozen()
    {
        if (isFrozen)
            throw new InvalidOperationException("Cannot modify frozen metadata.");
    }

    public string Name { get; }

    public NamespaceMetadata Namespace { get; }

    public IReadOnlyCollection<PackageMetadata> Dependencies
        => dependencies;
}