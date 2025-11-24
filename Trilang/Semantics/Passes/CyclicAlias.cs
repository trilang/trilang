using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class CyclicAlias : ISemanticPass
{
    private readonly HashSet<ITypeMetadata> visitedTypes;

    public CyclicAlias()
        => visitedTypes = [];

    public void Analyze(IEnumerable<SemanticTree> _, SemanticPassContext context)
    {
        var typeProvider = context.RootMetadataProvider;
        foreach (var aliasType in typeProvider.Types.OfType<AliasMetadata>())
            CheckCircularReference(aliasType, context.Diagnostics);
    }

    private void CheckCircularReference(
        AliasMetadata aliasType,
        SemanticDiagnosticReporter diagnostics)
    {
        var metadata = aliasType as ITypeMetadata;
        visitedTypes.Clear();

        while (true)
        {
            if (metadata is not AliasMetadata alias)
                return;

            if (!visitedTypes.Add(alias))
            {
                aliasType.MarkAsInvalid();
                diagnostics.CyclicTypeAlias(aliasType);
                return;
            }

            metadata = alias.Type!;
        }
    }

    public string Name => nameof(CyclicAlias);

    public IEnumerable<string> DependsOn => [nameof(MetadataGenerator)];
}