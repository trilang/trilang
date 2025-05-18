using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics.MetadataGenerators;

internal class ArrayGenerator
{
    private record Item(TypeArrayMetadata Metadata, ArrayTypeNode Node);

    private readonly HashSet<Item> typesToProcess;

    public ArrayGenerator()
        => typesToProcess = [];

    public void CreateArrays(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsArray)
                continue;

            if (symbol.Node is not ArrayTypeNode arrayTypeNode)
                throw new SemanticAnalysisException($"Expected '{symbol.Name}' to have an ArrayTypeNode, but found '{symbol.Node.GetType().Name}' instead.");

            var typeProvider = symbol.Node.SymbolTable!.TypeProvider;
            var metadata = new TypeArrayMetadata();
            if (typeProvider.DefineType(symbol.Name, metadata))
                typesToProcess.Add(new Item(metadata, arrayTypeNode));
        }
    }

    public void PopulateArrays()
    {
        foreach (var (arrayMetadata, arrayTypeNode) in typesToProcess)
        {
            var typeProvider = arrayTypeNode.SymbolTable!.TypeProvider;
            var itemMetadata = typeProvider.GetType(arrayTypeNode.ElementType.Name) ??
                               throw new SemanticAnalysisException($"The '{arrayTypeNode.ElementType.Name}' array item type is not defined.");

            arrayMetadata.ItemMetadata = itemMetadata;
        }
    }
}