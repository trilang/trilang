using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class CyclicTypeAlias : ISemanticPass
{
    private readonly HashSet<ITypeMetadata> visitedTypes;

    public CyclicTypeAlias()
        => visitedTypes = [];

    public void Analyze(SemanticTree tree, SemanticPassContext context)
    {
        var typeProvider = context.RootSymbolTable.TypeProvider;
        foreach (var aliasType in typeProvider.Types.OfType<TypeAliasMetadata>())
            CheckCircularReference(aliasType, context.Diagnostics);
    }

    private void CheckCircularReference(
        TypeAliasMetadata aliasType,
        SemanticDiagnosticReporter diagnostics)
    {
        var metadata = aliasType as ITypeMetadata;
        visitedTypes.Clear();

        while (true)
        {
            if (metadata is not TypeAliasMetadata alias)
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

    public string Name => nameof(CyclicTypeAlias);

    public IEnumerable<string> DependsOn => [nameof(MetadataGenerator)];
}