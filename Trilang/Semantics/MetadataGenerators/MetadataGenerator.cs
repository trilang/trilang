using Trilang.Parsing;
using Trilang.Symbols;

namespace Trilang.Semantics.MetadataGenerators;

internal class MetadataGenerator : Visitor
{
    private readonly TypeGenerator typeGenerator;
    private readonly InterfaceGenerator interfaceGenerator;
    private readonly DiscriminatedUnionGenerator discriminatedUnionGenerator;
    private readonly AliasGenerator aliasGenerator;
    private readonly TupleGenerator tupleGenerator;
    private readonly ArrayGenerator arrayGenerator;
    private readonly GenericTypeGenerator genericTypeGenerator;
    private readonly FunctionTypeGenerator functionTypeGenerator;
    private readonly FunctionGenerator functionGenerator;

    public MetadataGenerator(SymbolTableMap symbolTableMap)
    {
        typeGenerator = new TypeGenerator(symbolTableMap);
        interfaceGenerator = new InterfaceGenerator(symbolTableMap);
        discriminatedUnionGenerator = new DiscriminatedUnionGenerator(symbolTableMap);
        aliasGenerator = new AliasGenerator(symbolTableMap);
        tupleGenerator = new TupleGenerator(symbolTableMap);
        arrayGenerator = new ArrayGenerator(symbolTableMap);
        genericTypeGenerator = new GenericTypeGenerator(symbolTableMap);
        functionTypeGenerator = new FunctionTypeGenerator(symbolTableMap);
        functionGenerator = new FunctionGenerator(symbolTableMap);
    }

    public void Generate(RootSymbolTable rootSymbolTable)
    {
        var types = rootSymbolTable.Types;
        var ids = rootSymbolTable.Ids;

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
}