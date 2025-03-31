using Trilang.Symbols;

namespace Trilang.Semantics;

public class SymbolTableBuilderContext
{
    private bool noScope;

    public SymbolTableBuilderContext()
        : this(new SymbolTable())
    {
    }

    private SymbolTableBuilderContext(SymbolTable symbolTable)
    {
        SymbolTable = symbolTable;
        noScope = false;
    }

    public void Scoped(Action<SymbolTableBuilderContext> action)
    {
        var child = noScope
            ? SymbolTable
            : SymbolTable.CreateChild();
        var context = new SymbolTableBuilderContext(child)
        {
            noScope = false
        };

        action(context);
    }

    public void DisableNextScope()
        => noScope = true;

    public SymbolTable SymbolTable { get; }
}