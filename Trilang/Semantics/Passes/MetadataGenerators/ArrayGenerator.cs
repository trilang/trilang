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

            var arrayTypeNode = (ArrayType)symbol.Node;
            var root = arrayTypeNode.GetRoot();
            var typeProvider = symbolTableMap.Get(symbol.Node).TypeProvider;
            var metadata = new TypeArrayMetadata(
                new SourceLocation(root.SourceFile, arrayTypeNode.SourceSpan.GetValueOrDefault()));

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
                               TypeMetadata.Invalid(arrayTypeNode.ElementType.Name);

            arrayMetadata.ItemMetadata = itemMetadata;
        }
    }
}