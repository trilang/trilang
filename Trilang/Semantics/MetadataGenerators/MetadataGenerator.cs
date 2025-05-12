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
        var functionGenerator = new FunctionGenerator();

        typeGenerator.CreateTypes(symbolTable.Types);
        interfaceGenerator.CreateInterfaces(symbolTable.Types);
        discriminatedUnionGenerator.CreateDiscriminatedUnion(symbolTable.Types);
        aliasGenerator.CreateAliases(symbolTable.Types);
        tupleGenerator.CreateTuples(symbolTable.Types);
        arrayGenerator.CreateArrays(symbolTable.Types);
        genericTypeGenerator.CreateGenericTypes(symbolTable.Types);
        functionGenerator.BuildFunctionTypes(symbolTable.Types);

        aliasGenerator.PopulateAliases(symbolTable.Types);
        interfaceGenerator.PopulateInterfaces(symbolTable.Types);
        typeGenerator.PopulateTypes(symbolTable.Types);
        discriminatedUnionGenerator.PopulateDiscriminatedUnion(symbolTable.Types);
        tupleGenerator.PopulateTuples(symbolTable.Types);
        arrayGenerator.PopulateArrays(symbolTable.Types);
        genericTypeGenerator.PopulateGenericTypes(symbolTable.Types);
        functionGenerator.BuildFunctions(symbolTable.Ids);
    }

    protected override void VisitEnter(SyntaxTree node)
        => BuildSymbolTableTypes(node.SymbolTable);
}