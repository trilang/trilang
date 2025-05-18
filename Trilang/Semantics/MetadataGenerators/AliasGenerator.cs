using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics.MetadataGenerators;

internal class AliasGenerator
{
    private record Item(TypeAliasMetadata Metadata, TypeAliasDeclarationNode Node);

    private readonly HashSet<Item> typesToProcess;

    public AliasGenerator()
        => typesToProcess = [];

    public void CreateAliases(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsAlias)
                continue;

            if (symbol.Node is not TypeAliasDeclarationNode typeAliasNode)
                throw new SemanticAnalysisException("The type alias declaration is not valid.");

            var typeProvider = symbol.Node.SymbolTable!.TypeProvider;
            var alias = new TypeAliasMetadata(symbol.Name);
            if (!typeProvider.DefineType(symbol.Name, alias))
                throw new SemanticAnalysisException($"The '{symbol.Name}' type is already defined.");

            typesToProcess.Add(new Item(alias, typeAliasNode));
        }
    }

    public void PopulateAliases()
    {
        foreach (var (aliasMetadata, typeAliasNode) in typesToProcess)
        {
            var typeProvider = typeAliasNode.SymbolTable!.TypeProvider;
            var aliasedMetadata = typeProvider.GetType(typeAliasNode.Type.Name) ??
                                  throw new SemanticAnalysisException($"The '{typeAliasNode.Type.Name}' aliased type is not defined.");

            aliasMetadata.Type = aliasedMetadata;
        }
    }
}