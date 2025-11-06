using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes.MetadataGenerators;

internal class DiscriminatedUnionGenerator
{
    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly SymbolTableMap symbolTableMap;
    private readonly HashSet<DiscriminatedUnion> typesToProcess;

    public DiscriminatedUnionGenerator(
        SemanticDiagnosticReporter diagnostics,
        SymbolTableMap symbolTableMap)
    {
        this.diagnostics = diagnostics;
        this.symbolTableMap = symbolTableMap;
        typesToProcess = [];
    }

    public void CreateDiscriminatedUnion(IReadOnlyList<TypeSymbol> types)
    {
        foreach (var symbol in types)
        {
            if (!symbol.IsDiscriminatedUnion)
                continue;

            var typeProvider = symbolTableMap.Get(symbol.Node).TypeProvider;
            var node = (DiscriminatedUnion)symbol.Node;

            if (typeProvider.GetType(symbol.Name) is not DiscriminatedUnionMetadata metadata)
            {
                metadata = new DiscriminatedUnionMetadata(node.GetLocation());

                if (typeProvider.DefineType(symbol.Name, metadata))
                    typesToProcess.Add(node);
            }

            node.Metadata = metadata;
        }
    }

    public void PopulateDiscriminatedUnion()
    {
        foreach (var node in typesToProcess)
        {
            var metadata = (DiscriminatedUnionMetadata)node.Metadata!;
            var typeProvider = symbolTableMap.Get(node).TypeProvider;

            foreach (var typeNode in node.Types)
                metadata.AddType(typeNode.PopulateMetadata(typeProvider, diagnostics));
        }
    }
}