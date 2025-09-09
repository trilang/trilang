using Trilang.Semantics.Model;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes;

public class SymbolTableMap
{
    private readonly Dictionary<ISemanticNode, ISymbolTable> tables;

    public SymbolTableMap()
        => tables = new Dictionary<ISemanticNode, ISymbolTable>(new Comparer());

    public void Add(ISemanticNode node, ISymbolTable table)
        => tables.Add(node, table);

    public ISymbolTable Get(ISemanticNode node)
        => tables[node];

    private sealed class Comparer : IEqualityComparer<ISemanticNode>
    {
        public bool Equals(ISemanticNode? x, ISemanticNode? y)
            => ReferenceEquals(x, y);

        public int GetHashCode(ISemanticNode obj)
            => HashCode.Combine(obj);
    }
}