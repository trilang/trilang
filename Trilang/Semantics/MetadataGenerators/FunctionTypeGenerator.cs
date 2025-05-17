using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics.MetadataGenerators;

public class FunctionTypeGenerator
{
    private record Item(FunctionTypeMetadata Metadata, FunctionTypeNode Node);

    private readonly HashSet<Item> typesToProcess;

    public FunctionTypeGenerator()
        => typesToProcess = [];

    public void CreateFunctionTypes(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsFunction)
                continue;

            if (symbol.Node is not FunctionTypeNode function)
                throw new SemanticAnalysisException($"The '{symbol.Name}' symbol is not a function.");

            var typeProvider = symbol.Node.SymbolTable!.TypeProvider;
            var functionTypeMetadata = new FunctionTypeMetadata(symbol.Name);
            if (typeProvider.DefineType(functionTypeMetadata))
                typesToProcess.Add(new Item(functionTypeMetadata, function));
        }
    }

    public void PopulateFunctionTypes()
    {
        foreach (var (functionTypeMetadata, functionTypeNode) in typesToProcess)
        {
            // TODO: generic?
            var typeProvider = functionTypeNode.SymbolTable!.TypeProvider;

            foreach (var parameterType in functionTypeNode.ParameterTypes)
            {
                var parameter = typeProvider.GetType(parameterType.Name) ??
                                throw new SemanticAnalysisException($"The function has unknown parameter type: '{parameterType.Name}'.");

                functionTypeMetadata.AddParameter(parameter);
            }

            var returnType = typeProvider.GetType(functionTypeNode.ReturnType.Name) ??
                             throw new SemanticAnalysisException($"The function has unknown return type: '{functionTypeNode.ReturnType.Name}'.");


            functionTypeMetadata.ReturnType = returnType;
        }
    }
}