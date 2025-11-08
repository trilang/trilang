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
            foreach (var node in symbol.Nodes)
            {
                if (node is not FunctionDeclaration function)
                    continue;

                var typeProvider = symbolTableMap.Get(node).TypeProvider;
                var parameterTypes = string.Join(", ", function.Parameters.Select(p => p.Type.Name));
                var name = $"({parameterTypes}) => {function.ReturnType.Name}";
                if (typeProvider.GetType(name) is not FunctionTypeMetadata functionTypeMetadata)
                {
                    functionTypeMetadata = new FunctionTypeMetadata(function.GetLocation());

                    if (typeProvider.DefineType(name, functionTypeMetadata))
                        typesToProcess.Add(function);
                }

                var metadata = new FunctionMetadata(
                    function.GetLocation(),
                    function.AccessModifier.ToMetadata(),
                    function.Name,
                    [],
                    functionTypeMetadata);

                var isDefined = typeProvider.Functions.Any(x => x.Name == metadata.Name);
                if (isDefined)
                {
                    metadata.MarkAsInvalid();
                    diagnostics.FunctionAlreadyDefined(function);
                }

                typeProvider.AddFunction(metadata);

                function.Metadata = metadata;
            }
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

                var isDefined = metadata.Parameters.Any(x => x.Name == parameter.Name);
                if (isDefined)
                {
                    parameter.MarkAsInvalid();
                    diagnostics.ParameterAlreadyDefined(functionParameter);
                }

                functionParameter.Metadata = parameter;
                functionTypeMetadata.AddParameter(parameterType);
                metadata.AddParameter(parameter);
            }

            functionTypeMetadata.ReturnType = function.ReturnType.PopulateMetadata(typeProvider, diagnostics);
        }
    }
}