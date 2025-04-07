namespace Trilang.Symbols;

public class SymbolTable : ISymbolTable, IEquatable<SymbolTable>
{
    private readonly ISymbolTable parent;
    private readonly Dictionary<string, FunctionSymbol> functions;
    private readonly Dictionary<string, VariableSymbol> variables;

    public SymbolTable(ISymbolTable parent)
    {
        this.parent = parent;
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

        return functions.DictionaryEquals(other.functions) &&
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
        => HashCode.Combine(parent, variables);

    public TypeSymbol? GetType(string name)
        => parent.GetType(name);

    public bool TryAddType(TypeSymbol symbol)
        => parent.TryAddType(symbol);

    public FunctionSymbol? GetFunction(string name)
        => functions.TryGetValue(name, out var function)
            ? function
            : parent.GetFunction(name);

    public bool TryAddFunction(FunctionSymbol symbol)
        => functions.TryAdd(symbol.Name, symbol);

    public VariableSymbol? GetVariable(string name)
        => variables.TryGetValue(name, out var symbol)
            ? symbol
            : parent.GetVariable(name);

    public bool TryAddVariable(VariableSymbol symbol)
        => variables.TryAdd(symbol.Name, symbol);

    public ISymbolTable CreateChild()
        => new SymbolTable(this);

    public IReadOnlyDictionary<string, TypeSymbol> Types
        => parent.Types;

    public IReadOnlyDictionary<string, FunctionSymbol> FunctionsInScope
        => functions;

    public IReadOnlyDictionary<string, VariableSymbol> VariablesInScope
        => variables;
}