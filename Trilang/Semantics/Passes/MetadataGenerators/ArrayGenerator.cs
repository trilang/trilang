using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes.MetadataGenerators;

internal class ArrayGenerator
{
    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly SymbolTableMap symbolTableMap;
    private readonly HashSet<ArrayType> typesToProcess;

    public ArrayGenerator(SemanticDiagnosticReporter diagnostics, SymbolTableMap symbolTableMap)
    {
        this.diagnostics = diagnostics;
        this.symbolTableMap = symbolTableMap;
        typesToProcess = [];
    }

    public void CreateArrays(IReadOnlyList<TypeSymbol> types)
    {
        foreach (var symbol in types)
        {
            if (!symbol.IsArray)
                continue;

            var typeProvider = symbolTableMap.Get(symbol.Node).TypeProvider;
            var node = (ArrayType)symbol.Node;

            if (typeProvider.GetType(symbol.Name) is not ArrayMetadata metadata)
            {
                metadata = new ArrayMetadata(node.GetLocation());

                if (typeProvider.DefineType(symbol.Name, metadata))
                    typesToProcess.Add(node);
            }

            node.Metadata = metadata;
        }
    }

    public void PopulateArrays()
    {
        foreach (var arrayTypeNode in typesToProcess)
        {
            var metadata = (ArrayMetadata)arrayTypeNode.Metadata!;
            var typeProvider = symbolTableMap.Get(arrayTypeNode).TypeProvider;
            var itemMetadata = arrayTypeNode.ElementType.PopulateMetadata(typeProvider, diagnostics);

            metadata.ItemMetadata = itemMetadata;
        }
    }
}