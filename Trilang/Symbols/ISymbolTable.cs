namespace Trilang.Symbols;

public interface ISymbolTable
{
    TypeSymbol? GetType(string name);
    bool TryAddType(TypeSymbol symbol);

    FunctionSymbol? GetFunction(string name);
    bool TryAddFunction(FunctionSymbol symbol);

    VariableSymbol? GetVariable(string name);
    bool TryAddVariable(VariableSymbol symbol);

    ISymbolTable CreateChild();

    IReadOnlyDictionary<string, TypeSymbol> Types { get; }
    IReadOnlyDictionary<string, FunctionSymbol> FunctionsInScope { get; }
    IReadOnlyDictionary<string, VariableSymbol> VariablesInScope { get; }
}