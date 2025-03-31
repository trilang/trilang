using Trilang.Parsing.Ast;

namespace Trilang.Symbols;

public class FunctionSymbol : Symbol<FunctionDeclarationNode>
{
    public FunctionSymbol(FunctionDeclarationNode node)
        : base(SymbolKind.Function, node.Name, node)
    {
    }
}