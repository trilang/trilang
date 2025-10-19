using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes.MetadataGenerators;

internal class DiscriminatedUnionGenerator
{
    private record Item(DiscriminatedUnionMetadata Metadata, DiscriminatedUnion Node);

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

            var discriminatedUnionNode = (DiscriminatedUnion)symbol.Node;
            var root = discriminatedUnionNode.GetRoot();
            var typeProvider = symbolTableMap.Get(symbol.Node).TypeProvider;
            var metadata = new DiscriminatedUnionMetadata(
                new SourceLocation(root.SourceFile, discriminatedUnionNode.SourceSpan.GetValueOrDefault()));

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
                           TypeMetadata.Invalid(typeNode.Name);

                metadata.AddType(type);
            }
        }
    }
}