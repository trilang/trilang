using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes.MetadataGenerators;

internal class FunctionGenerator
{
    private record Item(FunctionTypeMetadata Metadata, FunctionDeclaration Node);

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
            if (symbol.Node is not FunctionDeclaration function)
                continue;

            var root = function.GetRoot();
            var typeProvider = symbolTableMap.Get(symbol.Node).TypeProvider;

            var parameters = string.Join(", ", function.Parameters.Select(p => p.Type.Name));
            var name = $"({parameters}) => {function.ReturnType.Name}";
            var functionTypeMetadata = new FunctionTypeMetadata(
                new SourceLocation(root.SourceFile, function.SourceSpan.GetValueOrDefault()));

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
                                TypeMetadata.Invalid(functionParameter.Type.Name);

                functionTypeMetadata.AddParameter(parameter);
            }

            functionTypeMetadata.ReturnType = typeProvider.GetType(function.ReturnType.Name) ??
                                              TypeMetadata.Invalid(function.ReturnType.Name);
        }
    }
}