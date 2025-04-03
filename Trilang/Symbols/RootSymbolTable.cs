namespace Trilang.Symbols;

public class RootSymbolTable : ISymbolTable, IEquatable<RootSymbolTable>
{
    private readonly Dictionary<string, TypeSymbol> types;
    private readonly Dictionary<string, FunctionSymbol> functions;
    private readonly Dictionary<string, VariableSymbol> variables;

    public RootSymbolTable()
    {
        types = new Dictionary<string, TypeSymbol>();
        functions = new Dictionary<string, FunctionSymbol>();
        variables = new Dictionary<string, VariableSymbol>();
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
               functions.DictionaryEquals(other.functions) &&
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
        => HashCode.Combine(types, functions, variables);

    public TypeSymbol? GetType(string name)
        => types.GetValueOrDefault(name);

    public bool TryAddType(TypeSymbol symbol)
        => types.TryAdd(symbol.Name, symbol);

    public FunctionSymbol? GetFunction(string name)
        => functions.GetValueOrDefault(name);

    public bool TryAddFunction(FunctionSymbol symbol)
        => functions.TryAdd(symbol.Name, symbol);

    public VariableSymbol? GetVariable(string name)
        => variables.GetValueOrDefault(name);

    public bool TryAddVariable(VariableSymbol symbol)
        => variables.TryAdd(symbol.Name, symbol);

    public ISymbolTable CreateChild()
        => new SymbolTable(this);

    public IReadOnlyDictionary<string, TypeSymbol> Types
        => types;

    public IReadOnlyDictionary<string, FunctionSymbol> Functions
        => functions;

    public IReadOnlyDictionary<string, VariableSymbol> VariablesInScope
        => variables;
}