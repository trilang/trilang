using Trilang.Metadata;

namespace Tri.Tests;

internal static class MetadataExtensions
{
    public static ITypeMetadata? FindType(this NamespaceMetadata ns, string name)
        => ns.Types.FirstOrDefault(x => x.ToString() == name);
}