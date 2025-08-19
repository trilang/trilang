using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics;

public class SymbolTableMap
{
    private readonly Dictionary<ISyntaxNode, ISymbolTable> tables;

    public SymbolTableMap()
        => tables = new Dictionary<ISyntaxNode, ISymbolTable>(new Comparer());

    public void Add(ISyntaxNode node, ISymbolTable table)
        => tables.Add(node, table);

    public ISymbolTable Get(ISyntaxNode node)
        => tables[node];

    private sealed class Comparer : IEqualityComparer<ISyntaxNode>
    {
        public bool Equals(ISyntaxNode? x, ISyntaxNode? y)
            => ReferenceEquals(x, y);

        public int GetHashCode(ISyntaxNode obj)
            => HashCode.Combine(obj);
    }
}