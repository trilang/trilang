using Trilang.Semantics.Model;
using Trilang.Semantics.Passes.MetadataGenerators;

namespace Trilang.Semantics.Passes;

internal class MetadataGenerator : ISemanticPass
{
    public void Analyze(IEnumerable<SemanticTree> semanticTrees, SemanticPassContext context)
    {
        var rootSymbolTable = context.RootSymbolTable;
        var types = rootSymbolTable.Types;
        var ids = rootSymbolTable.Ids;
        var symbolTableMap = context.SymbolTableMap!;
        var typeProviderMap = context.TypeProviderMap!;

        var typeGenerator = new TypeGenerator(context.Diagnostics, typeProviderMap);
        var interfaceGenerator = new InterfaceGenerator(context.Diagnostics, typeProviderMap);
        var discriminatedUnionGenerator = new DiscriminatedUnionGenerator(context.Diagnostics, typeProviderMap);
        var aliasGenerator = new AliasGenerator(context.Diagnostics, typeProviderMap);
        var tupleGenerator = new TupleGenerator(context.Diagnostics, typeProviderMap);
        var arrayGenerator = new ArrayGenerator(context.Diagnostics, typeProviderMap);
        var genericTypeGenerator = new GenericTypeGenerator(context.Diagnostics, typeProviderMap);
        var functionTypeGenerator = new FunctionTypeGenerator(context.Diagnostics, typeProviderMap);
        var functionGenerator = new FunctionGenerator(context.Diagnostics, typeProviderMap);

        typeGenerator.CreateTypes(types);
        interfaceGenerator.CreateInterfaces(types);
        discriminatedUnionGenerator.CreateDiscriminatedUnion(types);
        aliasGenerator.CreateAliases(types);
        tupleGenerator.CreateTuples(types);
        arrayGenerator.CreateArrays(types);
        functionTypeGenerator.CreateFunctionTypes(types);
        functionGenerator.CreateFunctions(ids);
        genericTypeGenerator.CreateGenericTypes(types);

        aliasGenerator.PopulateAliases();
        interfaceGenerator.PopulateInterfaces();
        typeGenerator.PopulateTypes();
        discriminatedUnionGenerator.PopulateDiscriminatedUnion();
        tupleGenerator.PopulateTuples();
        arrayGenerator.PopulateArrays();
        functionTypeGenerator.PopulateFunctionTypes();
        functionGenerator.PopulateFunctions();
        genericTypeGenerator.PopulateGenericTypes();

        var variableGenerator = new VariableGenerator(
            context.Diagnostics,
            symbolTableMap,
            typeProviderMap);

        foreach (var semanticTree in semanticTrees)
            semanticTree.Accept(variableGenerator);
    }

    public string Name => nameof(MetadataGenerator);

    public IEnumerable<string> DependsOn =>
    [
        nameof(SymbolFinder),
        nameof(MetadataProviderAnalyzer),
    ];
}