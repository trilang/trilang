namespace Trilang.Metadata;

public class CompilationContext
{
    private readonly HashSet<PackageMetadata> packages;

    public CompilationContext(BuiltInTypes builtInTypes, RootNamespaceMetadata rootNamespace)
    {
        packages = new HashSet<PackageMetadata>(PackageMetadataComparer.Instance);
        BuiltInTypes = builtInTypes;
        RootNamespace = rootNamespace;
    }

    public FindNamespaceResult FindNamespace(IEnumerable<string> parts)
        => FindNamespace(null, parts);

    public FindNamespaceResult FindNamespace(string? package, IEnumerable<string> parts)
    {
        var packageMetadata = package is not null
            ? packages.FirstOrDefault(p => p.Name == package)
            : CurrentPackage;

        if (packageMetadata is null)
            return FindNamespaceResult.PackageNotFound();

        return packageMetadata.FindNamespace(parts);
    }

    public PackageMetadata? GetPackage(string name)
        => packages.FirstOrDefault(p => p.Name == name);

    public void AddPackage(PackageMetadata package)
    {
        if (!packages.Add(package))
            throw new InvalidOperationException($"Package '{package.Name}' already exists.");
    }

    public IReadOnlyCollection<PackageMetadata> Packages
        => packages;

    public BuiltInTypes BuiltInTypes { get; }

    public RootNamespaceMetadata RootNamespace { get; }

    public PackageMetadata? CurrentPackage { get; set; }

    private sealed class PackageMetadataComparer : IEqualityComparer<PackageMetadata>
    {
        public static readonly PackageMetadataComparer Instance = new PackageMetadataComparer();

        public bool Equals(PackageMetadata? x, PackageMetadata? y)
        {
            if (ReferenceEquals(x, y))
                return true;

            if (x is null)
                return false;

            if (y is null)
                return false;

            if (x.GetType() != y.GetType())
                return false;

            return x.Name == y.Name;
        }

        public int GetHashCode(PackageMetadata obj)
            => obj.Name.GetHashCode();
    }
}