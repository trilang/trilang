using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics.MetadataGenerators;

internal class AliasGenerator
{
    public void CreateAliases(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (symbol is not { IsAlias: true, Node: TypeAliasDeclarationNode })
                continue;

            var typeProvider = symbol.Node.SymbolTable!.TypeProvider;
            var alias = new TypeAliasMetadata(symbol.Name);
            if (!typeProvider.DefineType(alias))
                throw new SemanticAnalysisException($"The '{symbol.Name}' type is already defined.");
        }
    }

    public void PopulateAliases(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        // all array and alias types are processed after all types are defined to support forward references
        foreach (var (_, symbol) in types)
        {
            if (symbol is not { IsAlias: true, Node: TypeAliasDeclarationNode typeAliasNode })
                continue;

            var typeProvider = symbol.Node.SymbolTable!.TypeProvider;
            var type = typeProvider.GetType(typeAliasNode.Name);
            if (type is not TypeAliasMetadata aliasMetadata)
                throw new SemanticAnalysisException($"The '{typeAliasNode.Name}' type is not an alias.");

            var aliasedMetadata = typeProvider.GetType(typeAliasNode.Type.Name) ??
                                  throw new SemanticAnalysisException($"The '{typeAliasNode.Type.Name}' aliased type is not defined.");

            aliasMetadata.Type = aliasedMetadata;
        }
    }
}