namespace Trilang.Symbols;

public class RootSymbolTable : ISymbolTable, IEquatable<RootSymbolTable>
{
    private readonly Dictionary<string, TypeSymbol> types;
    private readonly Dictionary<string, IdSymbol> variables;

    public RootSymbolTable()
    {
        types = new Dictionary<string, TypeSymbol>();
        variables = new Dictionary<string, IdSymbol>();
    }

    public static bool operator ==(RootSymbolTable? left, RootSymbolTable? right)
        => Equals(left, right);

    public static bool operator !=(RootSymbolTable? left, RootSymbolTable? right)
        => !Equals(left, right);

    public bool Equals(RootSymbolTable? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return types.DictionaryEquals(other.types) &&
               variables.DictionaryEquals(other.variables);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((RootSymbolTable)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(types, variables);

    public TypeSymbol? GetType(string name)
        => types.GetValueOrDefault(name);

    public bool TryAddType(TypeSymbol symbol)
        => types.TryAdd(symbol.Name, symbol);

    public IdSymbol? GetId(string name)
        => variables.GetValueOrDefault(name);

    public bool TryAddId(IdSymbol symbol)
        => variables.TryAdd(symbol.Name, symbol);

    public ISymbolTable CreateChild()
        => new SymbolTable(this);

    public IReadOnlyDictionary<string, TypeSymbol> Types
        => types;

    public IReadOnlyDictionary<string, IdSymbol> Ids
        => variables;
}