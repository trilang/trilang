using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes.MetadataGenerators;

internal class FunctionGenerator
{
    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly SymbolTableMap symbolTableMap;
    private readonly HashSet<FunctionDeclaration> typesToProcess;

    public FunctionGenerator(SemanticDiagnosticReporter diagnostics, SymbolTableMap symbolTableMap)
    {
        this.diagnostics = diagnostics;
        this.symbolTableMap = symbolTableMap;
        typesToProcess = [];
    }

    public void CreateFunctions(IReadOnlyDictionary<string, IdSymbol> functions)
    {
        foreach (var (_, symbol) in functions)
        {
            if (symbol.Node is not FunctionDeclaration node)
                continue;

            var typeProvider = symbolTableMap.Get(symbol.Node).TypeProvider;
            var parameterTypes = string.Join(", ", node.Parameters.Select(p => p.Type.Name));
            var name = $"({parameterTypes}) => {node.ReturnType.Name}";
            if (typeProvider.GetType(name) is not FunctionTypeMetadata metadata)
            {
                metadata = new FunctionTypeMetadata(node.GetLocation());

                if (typeProvider.DefineType(name, metadata))
                    typesToProcess.Add(node);
            }

            node.Metadata = new FunctionMetadata(
                node.GetLocation(),
                node.AccessModifier.ToMetadata(),
                node.Name,
                [],
                metadata);
        }
    }

    public void PopulateFunctions()
    {
        foreach (var function in typesToProcess)
        {
            var root = function.GetRoot();
            var metadata = function.Metadata!;
            var functionTypeMetadata = metadata.Type;
            var typeProvider = symbolTableMap.Get(function).TypeProvider;

            foreach (var functionParameter in function.Parameters)
            {
                var parameterType = functionParameter.Type.PopulateMetadata(typeProvider, diagnostics);
                var parameter = new ParameterMetadata(
                    new SourceLocation(root.SourceFile, functionParameter.SourceSpan.GetValueOrDefault()),
                    functionParameter.Name,
                    parameterType);

                functionParameter.Metadata = parameter;
                functionTypeMetadata.AddParameter(parameterType);
                metadata.AddParameter(parameter);
            }

            functionTypeMetadata.ReturnType = function.ReturnType.PopulateMetadata(typeProvider, diagnostics);
        }
    }
}