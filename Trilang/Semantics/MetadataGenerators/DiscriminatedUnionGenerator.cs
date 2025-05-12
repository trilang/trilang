using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics.MetadataGenerators;

internal class DiscriminatedUnionGenerator
{
    public void CreateDiscriminatedUnion(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsDiscriminatedUnion)
                continue;

            var typeProvider = symbol.Node.SymbolTable!.TypeProvider;
            var metadata = new DiscriminatedUnionMetadata(symbol.Name);
            typeProvider.DefineType(metadata);
        }
    }

    public void PopulateDiscriminatedUnion(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsDiscriminatedUnion)
                continue;

            var node = symbol.Node;
            if (node is not DiscriminatedUnionNode discriminatedUnionNode)
                continue;

            var typeProvider = node.SymbolTable!.TypeProvider;
            if (typeProvider.GetType(symbol.Name) is not DiscriminatedUnionMetadata metadata)
                throw new SemanticAnalysisException($"The '{symbol.Name}' type is not a discriminated union.");

            foreach (var typeNode in discriminatedUnionNode.Types)
            {
                var type = typeProvider.GetType(typeNode.Name) ??
                           throw new SemanticAnalysisException($"The '{typeNode.Name}' type is not defined.");

                metadata.AddType(type);
            }
        }
    }
}