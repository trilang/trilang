using Trilang.Parsing.Ast;

namespace Trilang.Symbols;

public class VariableSymbol : Symbol<VariableDeclarationNode>
{
    public VariableSymbol(VariableDeclarationNode node)
        : base(SymbolKind.Variable, node.Name, node)
    {
    }
}