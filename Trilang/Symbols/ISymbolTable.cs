using Trilang.Metadata;

namespace Trilang.Symbols;

public interface ISymbolTable
{
    IdSymbol? GetId(string name);
    bool TryAddId(IdSymbol symbol);
    void AddType(TypeSymbol symbol);

    ISymbolTable CreateChild();

    IReadOnlyList<TypeSymbol> Types { get; }
    IReadOnlyDictionary<string, IdSymbol> Ids { get; }
    ITypeMetadataProvider TypeProvider { get; }
}