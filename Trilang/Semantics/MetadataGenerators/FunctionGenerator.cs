using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics.MetadataGenerators;

internal class FunctionGenerator
{
    private record Item(FunctionTypeMetadata Metadata, FunctionDeclarationNode Node);

    private readonly SymbolTableMap symbolTableMap;
    private readonly HashSet<Item> typesToProcess;

    public FunctionGenerator(SymbolTableMap symbolTableMap)
    {
        this.symbolTableMap = symbolTableMap;
        typesToProcess = [];
    }

    public void CreateFunctions(IReadOnlyDictionary<string, IdSymbol> functions)
    {
        foreach (var (_, symbol) in functions)
        {
            if (symbol.Node is not FunctionDeclarationNode function)
                continue;

            var typeProvider = symbolTableMap.Get(symbol.Node).TypeProvider;

            var parameters = string.Join(", ", function.Parameters.Select(p => p.Type.Name));
            var name = $"({parameters}) => {function.ReturnType.Name}";
            var functionTypeMetadata = new FunctionTypeMetadata();
            if (typeProvider.DefineType(name, functionTypeMetadata))
                typesToProcess.Add(new Item(functionTypeMetadata, function));
        }
    }

    public void PopulateFunctions()
    {
        foreach (var (functionTypeMetadata, function) in typesToProcess)
        {
            var typeProvider = symbolTableMap.Get(function).TypeProvider;

            foreach (var functionParameter in function.Parameters)
            {
                var parameter = typeProvider.GetType(functionParameter.Type.Name) ??
                                throw new SemanticAnalysisException($"The function has unknown parameter type: '{functionParameter.Type.Name}'.");

                functionTypeMetadata.AddParameter(parameter);
            }

            var returnType = typeProvider.GetType(function.ReturnType.Name) ??
                             throw new SemanticAnalysisException($"The function has unknown return type: '{function.ReturnType.Name}'.");

            functionTypeMetadata.ReturnType = returnType;
        }
    }
}