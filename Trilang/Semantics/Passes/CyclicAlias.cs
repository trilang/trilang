using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class CyclicAlias : ISemanticPass
{
    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly NamespaceMetadata rootNamespace;
    private readonly HashSet<ITypeMetadata> visitedTypes;

    public CyclicAlias(SemanticDiagnosticReporter diagnostics, NamespaceMetadata rootNamespace)
    {
        this.diagnostics = diagnostics;
        this.rootNamespace = rootNamespace;
        visitedTypes = [];
    }

    public void Analyze(IEnumerable<SemanticTree> _)
    {
        foreach (var aliasType in rootNamespace.EnumerateAllTypes().OfType<AliasMetadata>())
            CheckCircularReference(aliasType, diagnostics);
    }

    private void CheckCircularReference(AliasMetadata aliasType, SemanticDiagnosticReporter diagnostics)
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