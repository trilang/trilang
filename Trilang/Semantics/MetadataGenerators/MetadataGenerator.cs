using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics.MetadataGenerators;

internal class MetadataGenerator : Visitor
{
    private void BuildSymbolTableTypes(ISymbolTable? symbolTable)
    {
        if (symbolTable is null)
            throw new ArgumentNullException(nameof(symbolTable));

        var typeGenerator = new TypeGenerator();
        var interfaceGenerator = new InterfaceGenerator();
        var discriminatedUnionGenerator = new DiscriminatedUnionGenerator();
        var aliasGenerator = new AliasGenerator();
        var tupleGenerator = new TupleGenerator();
        var arrayGenerator = new ArrayGenerator();
        var genericTypeGenerator = new GenericTypeGenerator();
        var functionTypeGenerator = new FunctionTypeGenerator();
        var functionGenerator = new FunctionGenerator();

        typeGenerator.CreateTypes(symbolTable.Types);
        interfaceGenerator.CreateInterfaces(symbolTable.Types);
        discriminatedUnionGenerator.CreateDiscriminatedUnion(symbolTable.Types);
        aliasGenerator.CreateAliases(symbolTable.Types);
        tupleGenerator.CreateTuples(symbolTable.Types);
        arrayGenerator.CreateArrays(symbolTable.Types);
        functionTypeGenerator.CreateFunctionTypes(symbolTable.Types);
        functionGenerator.CreateFunctions(symbolTable.Ids);
        genericTypeGenerator.CreateGenericTypes(symbolTable.Types);

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

    protected override void VisitTreeEnter(SyntaxTree node)
        => BuildSymbolTableTypes(node.SymbolTable);
}