using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes.MetadataGenerators;

internal class FunctionTypeGenerator
{
    private record Item(FunctionTypeMetadata Metadata, FunctionType Node);

    private readonly SymbolTableMap symbolTableMap;
    private readonly HashSet<Item> typesToProcess;

    public FunctionTypeGenerator(SymbolTableMap symbolTableMap)
    {
        this.symbolTableMap = symbolTableMap;
        typesToProcess = [];
    }

    public void CreateFunctionTypes(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsFunction)
                continue;

            var function = (FunctionType)symbol.Node;
            var root = function.GetRoot();
            var typeProvider = symbolTableMap.Get(symbol.Node).TypeProvider;
            var functionTypeMetadata = new FunctionTypeMetadata(
                new SourceLocation(root.SourceFile, function.SourceSpan.GetValueOrDefault()));

            if (typeProvider.DefineType(symbol.Name, functionTypeMetadata))
                typesToProcess.Add(new Item(functionTypeMetadata, function));
        }
    }

    public void PopulateFunctionTypes()
    {
        foreach (var (functionTypeMetadata, functionTypeNode) in typesToProcess)
        {
            // TODO: generic?
            var typeProvider = symbolTableMap.Get(functionTypeNode).TypeProvider;

            foreach (var parameterType in functionTypeNode.ParameterTypes)
            {
                var parameter = typeProvider.GetType(parameterType.Name) ??
                                TypeMetadata.Invalid(parameterType.Name);

                functionTypeMetadata.AddParameter(parameter);
            }

            var returnType = typeProvider.GetType(functionTypeNode.ReturnType.Name) ??
                             TypeMetadata.Invalid(functionTypeNode.ReturnType.Name);

            functionTypeMetadata.ReturnType = returnType;
        }
    }
}