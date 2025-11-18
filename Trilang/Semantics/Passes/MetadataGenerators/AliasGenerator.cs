using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes.MetadataGenerators;

internal class AliasGenerator
{
    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly SymbolTableMap symbolTableMap;
    private readonly HashSet<AliasDeclaration> typesToProcess;

    public AliasGenerator(SemanticDiagnosticReporter diagnostics, SymbolTableMap symbolTableMap)
    {
        this.diagnostics = diagnostics;
        this.symbolTableMap = symbolTableMap;
        typesToProcess = [];
    }

    public void CreateAliases(IReadOnlyList<TypeSymbol> types)
    {
        foreach (var symbol in types)
        {
            if (!symbol.IsAlias)
                continue;

            var typeProvider = symbolTableMap.Get(symbol.Node).TypeProvider;
            var node = (AliasDeclaration)symbol.Node;
            var root = node.GetRoot();
            var metadata = new AliasMetadata(
                new SourceLocation(root.SourceFile, node.SourceSpan.GetValueOrDefault()),
                node.Name);

            foreach (var genericArgument in node.GenericArguments)
            {
                var argumentMetadata = new TypeArgumentMetadata(
                    new SourceLocation(root.SourceFile, genericArgument.SourceSpan.GetValueOrDefault()),
                    genericArgument.Name) as ITypeMetadata;

                if (!typeProvider.DefineType(genericArgument.Name, argumentMetadata))
                {
                    argumentMetadata = TypeArgumentMetadata.Invalid(genericArgument.Name);
                    diagnostics.TypeArgumentAlreadyDefined(genericArgument);
                }

                metadata.AddGenericArgument(argumentMetadata);
                genericArgument.Metadata = argumentMetadata;
            }

            if (!typeProvider.DefineType(symbol.Name, metadata))
            {
                diagnostics.TypeAlreadyDefined(node);
                metadata.MarkAsInvalid();
            }

            node.Metadata = metadata;

            typesToProcess.Add(node);
        }
    }

    public void PopulateAliases()
    {
        foreach (var typeAliasNode in typesToProcess)
        {
            var metadata = (AliasMetadata)typeAliasNode.Metadata!;
            var typeProvider = symbolTableMap.Get(typeAliasNode).TypeProvider;
            var aliasedType = typeAliasNode.Type;
            var aliasedMetadata = typeProvider.GetType(aliasedType.Name);
            if (aliasedMetadata is null && aliasedType is GenericType genericType)
                aliasedMetadata = typeProvider.GetType(genericType.GetOpenGenericName());

            if (aliasedMetadata is null)
            {
                aliasedMetadata = TypeMetadata.Invalid(aliasedType.Name);
                diagnostics.UnknownType(aliasedType);
            }

            metadata.Type = aliasedMetadata;
            aliasedType.Metadata = aliasedMetadata;
        }
    }
}