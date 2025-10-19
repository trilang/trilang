using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes.MetadataGenerators;

internal class TupleGenerator
{
    private record Item(TupleMetadata Metadata, TupleType Node);

    private readonly SymbolTableMap symbolTableMap;
    private readonly HashSet<Item> typesToProcess;

    public TupleGenerator(SymbolTableMap symbolTableMap)
    {
        this.symbolTableMap = symbolTableMap;
        typesToProcess = [];
    }

    public void CreateTuples(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsTuple)
                continue;

            var tupleNode = (TupleType)symbol.Node;
            var root = tupleNode.GetRoot();
            var typeProvider = symbolTableMap.Get(symbol.Node).TypeProvider;
            var tuple = new TupleMetadata(
                new SourceLocation(root.SourceFile, tupleNode.SourceSpan.GetValueOrDefault()));

            if (typeProvider.DefineType(symbol.Name, tuple))
                typesToProcess.Add(new Item(tuple, tupleNode));
        }
    }

    public void PopulateTuples()
    {
        foreach (var (tuple, tupleNode) in typesToProcess)
        {
            var typeProvider = symbolTableMap.Get(tupleNode).TypeProvider;

            foreach (var typeNode in tupleNode.Types)
            {
                var type = typeProvider.GetType(typeNode.Name) ??
                           TypeMetadata.Invalid(typeNode.Name);

                tuple.AddType(type);
            }
        }
    }
}