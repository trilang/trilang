using Trilang.Semantics.Model;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes;

// TODO: add only unique tables?
public class SymbolTableMap
{
    private readonly Dictionary<ISemanticNode, ISymbolTable> tables;

    public SymbolTableMap()
        => tables = [];

    public void Add(ISemanticNode node, ISymbolTable table)
        => tables.Add(node, table);

    public ISymbolTable Get(ISemanticNode node)
        => tables[node];
}