using Trilang.Semantics.Model;

namespace Trilang.Symbols;

public interface ISymbol
{
    string Name { get; }

    ISemanticNode? Node { get; }
}