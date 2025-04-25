namespace Trilang.Symbols;

public interface ISymbolTable
{
    TypeSymbol? GetType(string name);
    bool TryAddType(TypeSymbol symbol);

    IdSymbol? GetId(string name);
    bool TryAddId(IdSymbol symbol);

    ISymbolTable CreateChild();

    IReadOnlyDictionary<string, TypeSymbol> Types { get; }
    IReadOnlyDictionary<string, IdSymbol> Ids { get; }
}