using Trilang.Parsing.Ast;

namespace Trilang.Symbols;

public interface ISymbol
{
    string Name { get; }

    ISyntaxNode? Node { get; }
}