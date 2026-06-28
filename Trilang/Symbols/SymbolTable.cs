namespace Trilang.Symbols;

public class SymbolTable
{
    private readonly SymbolTable? parent;
    private readonly List<IdSymbol> ids;

    public SymbolTable() : this(null)
    {
    }

    private SymbolTable(SymbolTable? parent)
    {
        this.parent = parent;
        ids = [];
    }

    public IReadOnlyList<IdSymbol> GetId(string name)
    {
        var symbols = ids.Where(id => id.Name == name).ToList();
        if (symbols.Count != 0)
            return symbols;

        return parent?.GetId(name) ?? [];
    }

    public IdSymbol AddId(string name)
    {
        var idSymbol = new IdSymbol(name);
        ids.Add(idSymbol);

        return idSymbol;
    }

    public SymbolTable CreateChild()
        => new SymbolTable(this);

    public IReadOnlyList<IdSymbol> Ids
        => ids;
}