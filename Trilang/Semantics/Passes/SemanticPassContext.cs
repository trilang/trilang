using Trilang.Symbols;

namespace Trilang.Semantics.Passes;

internal record SemanticPassContext(HashSet<string> Directives, RootSymbolTable RootSymbolTable)
{
    public SymbolTableMap? SymbolTableMap { get; set; }
}