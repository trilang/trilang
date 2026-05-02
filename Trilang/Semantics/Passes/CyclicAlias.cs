using Trilang.Compilation;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;

namespace Trilang.Semantics.Passes;

internal class CyclicAlias : ISemanticPass
{
    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly CompilationContext compilationContext;
    private readonly HashSet<ITypeMetadata> visitedTypes;

    public CyclicAlias(DiagnosticCollection diagnostics, CompilationContext compilationContext)
    {
        this.diagnostics = diagnostics.ForSemantic();
        this.compilationContext = compilationContext;
        visitedTypes = [];
    }

    public void Analyze(Project _)
    {
        var packageNamespace = compilationContext.CurrentPackage!.Namespace;

        foreach (var aliasType in packageNamespace.EnumerateAllTypes().OfType<AliasMetadata>())
            CheckCircularReference(aliasType);
    }

    private void CheckCircularReference(AliasMetadata aliasType)
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