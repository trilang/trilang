using Trilang.Symbols;

namespace Trilang.Semantics;

public class SymbolFinderContext
{
    private bool noScope;

    public SymbolFinderContext()
        : this(new RootSymbolTable())
    {
    }

    private SymbolFinderContext(ISymbolTable symbolTable)
    {
        SymbolTable = symbolTable;
        noScope = false;
    }

    public void Scoped(Action<SymbolFinderContext> action)
    {
        var child = noScope
            ? SymbolTable
            : SymbolTable.CreateChild();
        var context = new SymbolFinderContext(child)
        {
            noScope = false
        };

        action(context);
    }

    public void DisableNextScope()
        => noScope = true;

    public ISymbolTable SymbolTable { get; }
}