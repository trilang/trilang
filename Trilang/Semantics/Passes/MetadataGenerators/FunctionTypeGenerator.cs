using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes.MetadataGenerators;

internal class FunctionTypeGenerator
{
    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly MetadataProviderMap metadataProviderMap;
    private readonly HashSet<FunctionType> typesToProcess;

    public FunctionTypeGenerator(SemanticDiagnosticReporter diagnostics, MetadataProviderMap metadataProviderMap)
    {
        this.diagnostics = diagnostics;
        this.metadataProviderMap = metadataProviderMap;
        typesToProcess = [];
    }

    public void CreateFunctionTypes(IReadOnlyList<TypeSymbol> types)
    {
        foreach (var symbol in types)
        {
            if (!symbol.IsFunction)
                continue;

            var node = (FunctionType)symbol.Node;
            var typeProvider = metadataProviderMap.Get(node);

            if (typeProvider.GetType(symbol.Name) is not FunctionTypeMetadata metadata)
            {
                metadata = new FunctionTypeMetadata(node.GetLocation());

                if (typeProvider.DefineType(symbol.Name, metadata))
                    typesToProcess.Add(node);
            }

            node.Metadata = metadata;
        }
    }

    public void PopulateFunctionTypes()
    {
        foreach (var functionTypeNode in typesToProcess)
        {
            // TODO: generic?
            var metadata = (FunctionTypeMetadata)functionTypeNode.Metadata!;
            var typeProvider = metadataProviderMap.Get(functionTypeNode);

            foreach (var parameterType in functionTypeNode.ParameterTypes)
                metadata.AddParameter(parameterType.PopulateMetadata(typeProvider, diagnostics));

            metadata.ReturnType = functionTypeNode.ReturnType.PopulateMetadata(typeProvider, diagnostics);
        }
    }
}