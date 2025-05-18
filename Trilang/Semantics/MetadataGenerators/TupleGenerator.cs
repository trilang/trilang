using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics.MetadataGenerators;

internal class TupleGenerator
{
    private record Item(TupleMetadata Metadata, TupleTypeNode Node);

    private readonly HashSet<Item> typesToProcess;

    public TupleGenerator()
        => typesToProcess = [];

    public void CreateTuples(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsTuple)
                continue;

            if (symbol.Node is not TupleTypeNode tupleNode)
                throw new SemanticAnalysisException();

            var typeProvider = symbol.Node.SymbolTable!.TypeProvider;
            var tuple = new TupleMetadata();
            if (typeProvider.DefineType(symbol.Name, tuple))
                typesToProcess.Add(new Item(tuple, tupleNode));
        }
    }

    public void PopulateTuples()
    {
        foreach (var (tuple, tupleNode) in typesToProcess)
        {
            var typeProvider = tupleNode.SymbolTable!.TypeProvider;

            foreach (var typeNode in tupleNode.Types)
            {
                var type = typeProvider.GetType(typeNode.Name) ??
                           throw new SemanticAnalysisException($"The '{typeNode.Name}' type is not defined.");

                tuple.AddType(type);
            }
        }
    }
}