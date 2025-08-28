using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes.MetadataGenerators;

internal class TupleGenerator
{
    private record Item(TupleMetadata Metadata, TupleTypeNode Node);

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

            if (symbol.Node is not TupleTypeNode tupleNode)
                throw new SemanticAnalysisException();

            var typeProvider = symbolTableMap.Get(symbol.Node).TypeProvider;
            var tuple = new TupleMetadata();
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
                           throw new SemanticAnalysisException($"The '{typeNode.Name}' type is not defined.");

                tuple.AddType(type);
            }
        }
    }
}