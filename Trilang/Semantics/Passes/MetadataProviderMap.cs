using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

// TODO: add only unique providers?
public class MetadataProviderMap
{
    private readonly Dictionary<ISemanticNode, IMetadataProvider> providers;

    public MetadataProviderMap()
        => providers = [];

    public void Add(ISemanticNode node, IMetadataProvider provider)
        => providers.Add(node, provider);

    public IMetadataProvider Get(ISemanticNode node)
        => providers[node];
}