using Trilang.Symbols;

namespace Trilang.Semantics;

internal class SymbolFinderContext
{
    private bool noScope;

    public SymbolFinderContext(ISymbolTable symbolTable, SemanticAnalysisOptions semanticAnalysisOptions)
    {
        noScope = false;
        SymbolTable = symbolTable;
        SemanticAnalysisOptions = semanticAnalysisOptions;
    }

    public void Scoped(Action<SymbolFinderContext> action)
    {
        var child = noScope
            ? SymbolTable
            : SymbolTable.CreateChild();
        var context = new SymbolFinderContext(child, SemanticAnalysisOptions)
        {
            noScope = false
        };

        action(context);
    }

    public void DisableNextScope()
        => noScope = true;

    public ISymbolTable SymbolTable { get; }

    public SemanticAnalysisOptions SemanticAnalysisOptions { get; }
}