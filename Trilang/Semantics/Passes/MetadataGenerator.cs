using Trilang.Semantics.Model;
using Trilang.Semantics.Passes.MetadataGenerators;

namespace Trilang.Semantics.Passes;

internal class MetadataGenerator : ISemanticPass
{
    public void Analyze(IEnumerable<SemanticTree> _, SemanticPassContext context)
    {
        var rootSymbolTable = context.RootSymbolTable;
        var types = rootSymbolTable.Types;
        var ids = rootSymbolTable.Ids;
        var symbolTableMap = context.SymbolTableMap!;

        var typeGenerator = new TypeGenerator(context.Diagnostics, symbolTableMap);
        var interfaceGenerator = new InterfaceGenerator(context.Diagnostics, symbolTableMap);
        var discriminatedUnionGenerator = new DiscriminatedUnionGenerator(context.Diagnostics, symbolTableMap);
        var aliasGenerator = new AliasGenerator(context.Diagnostics, symbolTableMap);
        var tupleGenerator = new TupleGenerator(context.Diagnostics, symbolTableMap);
        var arrayGenerator = new ArrayGenerator(context.Diagnostics, symbolTableMap);
        var genericTypeGenerator = new GenericTypeGenerator(context.Diagnostics, symbolTableMap);
        var functionTypeGenerator = new FunctionTypeGenerator(context.Diagnostics, symbolTableMap);
        var functionGenerator = new FunctionGenerator(context.Diagnostics, symbolTableMap);

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
    }

    public string Name => nameof(MetadataGenerator);

    public IEnumerable<string> DependsOn => [nameof(SymbolFinder)];
}