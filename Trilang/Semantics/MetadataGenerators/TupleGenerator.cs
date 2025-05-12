using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics.MetadataGenerators;

internal class TupleGenerator
{
    public void CreateTuples(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsTuple)
                continue;

            var typeProvider = symbol.Node.SymbolTable!.TypeProvider;
            var tuple = new TupleMetadata(symbol.Name);
            typeProvider.DefineType(tuple);
        }
    }

    public void PopulateTuples(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsTuple)
                continue;

            if (symbol.Node is not TupleTypeNode tupleNode)
                throw new SemanticAnalysisException();

            var typeProvider = symbol.Node.SymbolTable!.TypeProvider;
            if (typeProvider.GetType(symbol.Name) is not TupleMetadata tuple)
                throw new SemanticAnalysisException($"The '{symbol.Name}' type is not a tuple.");

            if (tuple.Types.Count > 0)
                continue;

            foreach (var typeNode in tupleNode.Types)
            {
                var type = typeProvider.GetType(typeNode.Name) ??
                           throw new SemanticAnalysisException($"The '{typeNode.Name}' type is not defined.");

                tuple.AddType(type);
            }
        }
    }
}