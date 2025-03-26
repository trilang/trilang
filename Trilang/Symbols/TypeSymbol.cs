using Trilang.Parsing.Nodes;

namespace Trilang.Symbols;

public class TypeSymbol : Symbol<ISyntaxNode>
{
    // TODO:
    public TypeSymbol(string name, ISyntaxNode node)
        : base(SymbolKind.Type, name, node)
    {
    }
}