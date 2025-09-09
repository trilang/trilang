using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes.MetadataGenerators;

internal class ArrayGenerator
{
    private record Item(TypeArrayMetadata Metadata, ArrayType Node);

    private readonly SymbolTableMap symbolTableMap;
    private readonly HashSet<Item> typesToProcess;

    public ArrayGenerator(SymbolTableMap symbolTableMap)
    {
        this.symbolTableMap = symbolTableMap;
        typesToProcess = [];
    }

    public void CreateArrays(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsArray)
                continue;

            if (symbol.Node is not ArrayType arrayTypeNode)
                throw new SemanticAnalysisException($"Expected '{symbol.Name}' to have an ArrayTypeNode, but found '{symbol.Node.GetType().Name}' instead.");

            var typeProvider = symbolTableMap.Get(symbol.Node).TypeProvider;
            var metadata = new TypeArrayMetadata();
            if (typeProvider.DefineType(symbol.Name, metadata))
                typesToProcess.Add(new Item(metadata, arrayTypeNode));
        }
    }

    public void PopulateArrays()
    {
        foreach (var (arrayMetadata, arrayTypeNode) in typesToProcess)
        {
            var typeProvider = symbolTableMap.Get(arrayTypeNode).TypeProvider;
            var itemMetadata = typeProvider.GetType(arrayTypeNode.ElementType.Name) ??
                               throw new SemanticAnalysisException($"The '{arrayTypeNode.ElementType.Name}' array item type is not defined.");

            arrayMetadata.ItemMetadata = itemMetadata;
        }
    }
}