using Trilang.Parsing.Nodes;

namespace Trilang.Symbols;

public class VariableSymbol : Symbol<ISyntaxNode>
{
    public VariableSymbol(string name, ISyntaxNode node)
        : base(SymbolKind.Variable, name, node)
    {
    }
}