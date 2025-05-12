using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics.MetadataGenerators;

internal class ArrayGenerator
{
    public void CreateArrays(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsArray)
                continue;

            if (symbol.Node is not ArrayTypeNode arrayTypeNode)
                throw new SemanticAnalysisException();

            var metadata = new TypeArrayMetadata(arrayTypeNode.Name);
            var typeProvider = symbol.Node.SymbolTable!.TypeProvider;
            if (typeProvider.GetType(metadata.Name) is null)
                typeProvider.DefineType(metadata);
        }
    }

    public void PopulateArrays(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsArray)
                continue;

            if (symbol.Node is not ArrayTypeNode arrayTypeNode)
                throw new SemanticAnalysisException();

            var typeProvider = symbol.Node.SymbolTable!.TypeProvider;
            var type = typeProvider.GetType(arrayTypeNode.Name) as TypeArrayMetadata ??
                       throw new SemanticAnalysisException($"The '{arrayTypeNode.ElementType.Name}' array item type is not defined.");

            type.ItemMetadata = typeProvider.GetType(arrayTypeNode.ElementType.Name) ??
                                throw new SemanticAnalysisException($"The '{arrayTypeNode.ElementType.Name}' array item type is not defined.");
        }
    }
}