using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Semantics;

public class SemanticAnalysis
{
    public void Analyze(SyntaxTree tree)
    {
        tree.Accept(new SymbolFinder(), new SymbolFinderContext());
        tree.Accept(new VariableUsedBeforeDeclared(), new VisitorContext<object>());
        tree.Accept(new TypeChecker());
    }
}