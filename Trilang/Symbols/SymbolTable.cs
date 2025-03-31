namespace Trilang.Symbols;

public class SymbolTable : IEquatable<SymbolTable>
{
    private readonly SymbolTable? parent;

    private readonly Dictionary<string, TypeSymbol> types;
    private readonly Dictionary<string, FunctionSymbol> functions;
    private readonly Dictionary<string, VariableSymbol> variables;

    public SymbolTable()
        : this(null)
    {
    }

    private SymbolTable(SymbolTable? parent)
    {
        this.parent = parent;

        types = new Dictionary<string, TypeSymbol>();
        functions = new Dictionary<string, FunctionSymbol>();
        variables = new Dictionary<string, VariableSymbol>();
    }

    public static bool operator ==(SymbolTable? left, SymbolTable? right)
        => Equals(left, right);

    public static bool operator !=(SymbolTable? left, SymbolTable? right)
        => !Equals(left, right);

    public bool Equals(SymbolTable? other)
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

        return Equals((SymbolTable)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(parent, types, functions, variables);

    public TypeSymbol? GetType(string name)
        => types.TryGetValue(name, out var symbol)
            ? symbol
            : parent?.GetType(name);

    public bool TryAddType(TypeSymbol symbol)
        => types.TryAdd(symbol.Name, symbol);

    public FunctionSymbol? GetFunction(string name)
        => functions.TryGetValue(name, out var symbol)
            ? symbol
            : parent?.GetFunction(name);

    public bool TryAddFunction(FunctionSymbol symbol)
        => functions.TryAdd(symbol.Name, symbol);

    public VariableSymbol? GetVariable(string name)
        => variables.TryGetValue(name, out var symbol)
            ? symbol
            : parent?.GetVariable(name);

    public bool TryAddVariable(VariableSymbol symbol)
        => variables.TryAdd(symbol.Name, symbol);

    public SymbolTable CreateChild()
        => new SymbolTable(this);

    public IReadOnlyDictionary<string, TypeSymbol> Types
        => types;

    public IReadOnlyDictionary<string, FunctionSymbol> Functions
        => functions;

    public IReadOnlyDictionary<string, VariableSymbol> Variables
        => variables;
}