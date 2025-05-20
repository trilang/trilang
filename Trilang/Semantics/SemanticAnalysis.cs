using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Semantics.MetadataGenerators;
using Trilang.Symbols;

namespace Trilang.Semantics;

public class SemanticAnalysis
{
    public void Analyze(SyntaxTree tree)
    {
        var rootTypeProvider = new RootTypeMetadataProvider();
        var rootSymbolTable = new RootSymbolTable(rootTypeProvider);

        tree.Accept(new SymbolFinder(), new SymbolFinderContext(rootSymbolTable));
        tree.Accept(new VariableUsedBeforeDeclared());
        tree.Accept(new ThisOutsideOfClass());
        tree.Accept(new BreakContinueWithinLoop());
        tree.Accept(new MetadataGenerator());
        tree.Accept(new TypeChecker());
        tree.Accept(new NotImplementedInterface());
        tree.Accept(new CheckAccessModifiers());
        tree.Accept(new RecursiveTypeAlias());
    }
}