using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes.MetadataGenerators;

internal class DiscriminatedUnionGenerator
{
    private record Item(DiscriminatedUnionMetadata Metadata, DiscriminatedUnionNode Node);

    private readonly SymbolTableMap symbolTableMap;
    private readonly HashSet<Item> typesToProcess;

    public DiscriminatedUnionGenerator(SymbolTableMap symbolTableMap)
    {
        this.symbolTableMap = symbolTableMap;
        typesToProcess = [];
    }

    public void CreateDiscriminatedUnion(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsDiscriminatedUnion)
                continue;

            if (symbol.Node is not DiscriminatedUnionNode discriminatedUnionNode)
                throw new SemanticAnalysisException($"Expected '{symbol.Name}' to have a DiscriminatedUnionNode, but found '{symbol.Node.GetType().Name}' instead.");

            var typeProvider = symbolTableMap.Get(symbol.Node).TypeProvider;
            var metadata = new DiscriminatedUnionMetadata();
            if (typeProvider.DefineType(symbol.Name, metadata))
                typesToProcess.Add(new Item(metadata, discriminatedUnionNode));
        }
    }

    public void PopulateDiscriminatedUnion()
    {
        foreach (var (metadata, node) in typesToProcess)
        {
            var typeProvider = symbolTableMap.Get(node).TypeProvider;

            foreach (var typeNode in node.Types)
            {
                var type = typeProvider.GetType(typeNode.Name) ??
                           throw new SemanticAnalysisException($"The '{typeNode.Name}' type is not defined.");

                metadata.AddType(type);
            }
        }
    }
}