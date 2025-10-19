using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes.MetadataGenerators;

internal class AliasGenerator
{
    private record Item(TypeAliasMetadata Metadata, TypeAliasDeclaration Node);

    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly SymbolTableMap symbolTableMap;
    private readonly HashSet<Item> typesToProcess;

    public AliasGenerator(SemanticDiagnosticReporter diagnostics, SymbolTableMap symbolTableMap)
    {
        this.diagnostics = diagnostics;
        this.symbolTableMap = symbolTableMap;
        typesToProcess = [];
    }

    public void CreateAliases(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsAlias)
                continue;

            var typeAliasNode = (TypeAliasDeclaration)symbol.Node;
            var root = typeAliasNode.GetRoot();
            var typeProvider = symbolTableMap.Get(symbol.Node).TypeProvider;
            var alias = new TypeAliasMetadata(
                new SourceLocation(root.SourceFile, typeAliasNode.SourceSpan.GetValueOrDefault()),
                typeAliasNode.Name);

            foreach (var genericArgument in typeAliasNode.GenericArguments)
            {
                var argumentMetadata = new TypeArgumentMetadata(
                    new SourceLocation(root.SourceFile, genericArgument.SourceSpan.GetValueOrDefault()),
                    genericArgument.Name) as ITypeMetadata;

                if (!typeProvider.DefineType(genericArgument.Name, argumentMetadata))
                {
                    argumentMetadata = TypeArgumentMetadata.Invalid(genericArgument.Name);
                    diagnostics.TypeArgumentAlreadyDefined(genericArgument);
                }

                alias.AddGenericArgument(argumentMetadata);
            }

            if (typeProvider.DefineType(symbol.Name, alias))
                typesToProcess.Add(new Item(alias, typeAliasNode));
            else
                diagnostics.TypeAlreadyDefined(typeAliasNode);
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
                                  TypeMetadata.Invalid(aliasedMetadataName);

            aliasMetadata.Type = aliasedMetadata;
        }
    }
}