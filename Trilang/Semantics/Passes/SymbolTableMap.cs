using Trilang.Semantics.Model;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes;

// TODO: add only unique tables?
public class SymbolTableMap
{
    private readonly Dictionary<ISemanticNode, SymbolTable> tables;

    public SymbolTableMap()
        => tables = [];

    public void Add(ISemanticNode node, SymbolTable table)
        => tables.Add(node, table);

    public SymbolTable Get(ISemanticNode node)
        => tables[node];
}