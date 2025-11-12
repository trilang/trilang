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
            var group = new FunctionGroupMetadata();

            foreach (var node in symbol.Nodes)
            {
                if (node is not FunctionDeclaration function)
                    continue;

                var typeProvider = symbolTableMap.Get(node).TypeProvider;
                var metadata = new FunctionMetadata(
                    function.GetLocation(),
                    function.AccessModifier.ToMetadata(),
                    function.Name,
                    [],
                    null!,
                    group);

                typeProvider.AddFunction(metadata);

                function.Metadata = metadata;

                typesToProcess.Add(function);
            }
        }
    }

    public void PopulateFunctions()
    {
        foreach (var function in typesToProcess)
        {
            var root = function.GetRoot();
            var metadata = function.Metadata!;
            var typeProvider = symbolTableMap.Get(function).TypeProvider;

            foreach (var functionParameter in function.Parameters)
            {
                var parameter = new ParameterMetadata(
                    new SourceLocation(root.SourceFile, functionParameter.SourceSpan.GetValueOrDefault()),
                    functionParameter.Name,
                    functionParameter.Type.PopulateMetadata(typeProvider, diagnostics));

                var isParameterDefined = metadata.Parameters.Any(x => x.Name == parameter.Name);
                if (isParameterDefined)
                {
                    parameter.MarkAsInvalid();
                    diagnostics.ParameterAlreadyDefined(functionParameter);
                }

                functionParameter.Metadata = parameter;
                metadata.AddParameter(parameter);
            }

            metadata.Type = GetFunctionType(function, typeProvider);
        }

        foreach (var function in typesToProcess)
            CheckFunctionDuplicates(function);
    }

    private FunctionTypeMetadata GetFunctionType(
        FunctionDeclaration function,
        ITypeMetadataProvider typeProvider)
    {
        var metadata = function.Metadata!;
        var functionTypeMetadata = new FunctionTypeMetadata(
            null,
            metadata.Parameters.Select(x => x.Type),
            function.ReturnType.PopulateMetadata(typeProvider, diagnostics));

        if (typeProvider.GetType(functionTypeMetadata.ToString()) is not FunctionTypeMetadata existingFunctionType)
        {
            typeProvider.DefineType(functionTypeMetadata.ToString(), functionTypeMetadata);
            existingFunctionType = functionTypeMetadata;
        }

        return existingFunctionType;
    }

    private void CheckFunctionDuplicates(FunctionDeclaration function)
    {
        // TODO: use type provider to check for existing functions
        var metadata = function.Metadata!;
        var group = metadata.Group;
        if (ReferenceEquals(metadata, group.Functions[0]))
            return;

        var actualParameters = metadata.Parameters.Select(x => x.Type).ToArray();
        var isDefined = group.Functions
            .Any(x => !ReferenceEquals(x, metadata) &&
                      x.Type.ParameterTypes.SequenceEqual(actualParameters));

        if (isDefined)
        {
            metadata.MarkAsInvalid();
            diagnostics.FunctionAlreadyDefined(function);
        }
    }
}