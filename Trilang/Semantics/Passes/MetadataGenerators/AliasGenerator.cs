using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes.MetadataGenerators;

internal class AliasGenerator
{
    private record Item(TypeAliasMetadata Metadata, TypeAliasDeclarationNode Node);

    private readonly SymbolTableMap symbolTableMap;
    private readonly HashSet<Item> typesToProcess;

    public AliasGenerator(SymbolTableMap symbolTableMap)
    {
        this.symbolTableMap = symbolTableMap;
        typesToProcess = [];
    }

    public void CreateAliases(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsAlias)
                continue;

            if (symbol.Node is not TypeAliasDeclarationNode typeAliasNode)
                throw new SemanticAnalysisException("The type alias declaration is not valid.");

            var typeProvider = symbolTableMap.Get(symbol.Node).TypeProvider;
            var alias = new TypeAliasMetadata(typeAliasNode.Name);

            foreach (var genericArgument in typeAliasNode.GenericArguments)
            {
                var argumentMetadata = new TypeArgumentMetadata(genericArgument.Name);
                if (!typeProvider.DefineType(genericArgument.Name, argumentMetadata))
                    throw new SemanticAnalysisException($"The '{genericArgument.Name}' type argument is already defined.");

                alias.AddGenericArgument(argumentMetadata);
            }

            if (!typeProvider.DefineType(symbol.Name, alias))
                throw new SemanticAnalysisException($"The '{symbol.Name}' type is already defined.");

            typesToProcess.Add(new Item(alias, typeAliasNode));
        }
    }

    public void PopulateAliases()
    {
        foreach (var (aliasMetadata, typeAliasNode) in typesToProcess)
        {
            var typeProvider = symbolTableMap.Get(typeAliasNode).TypeProvider;
            var aliasedMetadata = typeProvider.GetType(typeAliasNode.Type.Name);
            if (aliasedMetadata is null && typeAliasNode.Type is GenericTypeNode genericTypeNode)
                aliasedMetadata = typeProvider.GetType(genericTypeNode.GetOpenGenericName());

            if (aliasedMetadata is null)
                throw new SemanticAnalysisException($"The '{typeAliasNode.Type.Name}' aliased type is not defined.");

            aliasMetadata.Type = aliasedMetadata;
        }
    }
}