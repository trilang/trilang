using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes.MetadataGenerators;

internal class TupleGenerator
{
    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly SymbolTableMap symbolTableMap;
    private readonly HashSet<TupleType> typesToProcess;

    public TupleGenerator(SemanticDiagnosticReporter diagnostics, SymbolTableMap symbolTableMap)
    {
        this.diagnostics = diagnostics;
        this.symbolTableMap = symbolTableMap;
        typesToProcess = [];
    }

    public void CreateTuples(IReadOnlyList<TypeSymbol> types)
    {
        foreach (var symbol in types)
        {
            if (!symbol.IsTuple)
                continue;

            var typeProvider = symbolTableMap.Get(symbol.Node).TypeProvider;
            var node = (TupleType)symbol.Node;

            if (typeProvider.GetType(symbol.Name) is not TupleMetadata metadata)
            {
                metadata = new TupleMetadata(node.GetLocation());

                if (typeProvider.DefineType(symbol.Name, metadata))
                    typesToProcess.Add(node);
            }

            node.Metadata = metadata;
        }
    }

    public void PopulateTuples()
    {
        foreach (var node in typesToProcess)
        {
            var tuple = (TupleMetadata)node.Metadata!;
            var typeProvider = symbolTableMap.Get(node).TypeProvider;

            foreach (var typeNode in node.Types)
                tuple.AddType(typeNode.PopulateMetadata(typeProvider, diagnostics));
        }
    }
}