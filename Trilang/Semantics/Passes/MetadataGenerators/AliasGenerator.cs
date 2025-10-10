using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes.MetadataGenerators;

internal class AliasGenerator
{
    private record Item(TypeAliasMetadata Metadata, TypeAliasDeclaration Node);

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

            if (symbol.Node is not TypeAliasDeclaration typeAliasNode)
                throw new SemanticAnalysisException("The type alias declaration is not valid.");

            var root = typeAliasNode.GetRoot();
            var typeProvider = symbolTableMap.Get(symbol.Node).TypeProvider;
            var alias = new TypeAliasMetadata(
                new SourceLocation(root.SourceFile, typeAliasNode.SourceSpan.GetValueOrDefault()),
                typeAliasNode.Name);

            foreach (var genericArgument in typeAliasNode.GenericArguments)
            {
                var argumentMetadata = new TypeArgumentMetadata(
                      new SourceLocation(root.SourceFile, genericArgument.SourceSpan.GetValueOrDefault()),
                      genericArgument.Name);

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
            var aliasedMetadataName = typeAliasNode.Type is GenericType genericTypeNode
                ? genericTypeNode.GetOpenGenericName()
                : typeAliasNode.Type.Name;

            var aliasedMetadata = typeProvider.GetType(aliasedMetadataName) ??
                                  throw new SemanticAnalysisException($"The '{typeAliasNode.Type.Name}' aliased type is not defined.");

            aliasMetadata.Type = aliasedMetadata;
        }
    }
}