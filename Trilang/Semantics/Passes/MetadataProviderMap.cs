using System.Diagnostics;
using Trilang.Semantics.Model;
using Trilang.Semantics.Providers;

namespace Trilang.Semantics.Passes;

public class MetadataProviderMap
{
    private readonly Dictionary<ISemanticNode, IMetadataProvider> providers;

    public MetadataProviderMap()
        => providers = new Dictionary<ISemanticNode, IMetadataProvider>(ReferenceEqualityComparer.Instance);

    public void Add(ISemanticNode node, IMetadataProvider provider)
    {
        Debug.Assert(!providers.ContainsValue(provider), "Provider is already added");

        providers[node] = provider;
    }

    public IMetadataProvider Get(ISemanticNode node)
    {
        while (true)
        {
            if (providers.TryGetValue(node, out var provider))
                return provider;

            node = node.Parent ??
                   throw new InvalidOperationException("Node has no parent");
        }
    }
}