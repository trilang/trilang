using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Symbols;

public interface ISymbolTable
{
    IdSymbol? GetId(string name);
    bool TryAddId(string name, ISemanticNode node);
    void AddType(TypeSymbol symbol);

    ISymbolTable CreateChild();

    IReadOnlyList<TypeSymbol> Types { get; }
    IReadOnlyDictionary<string, IdSymbol> Ids { get; }
    ITypeMetadataProvider TypeProvider { get; }
}