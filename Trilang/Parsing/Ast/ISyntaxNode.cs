using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public interface ISyntaxNode
{
    void Accept(IVisitor visitor);

    void Accept<TContext>(IVisitor<TContext> visitor, TContext context);

    ISyntaxNode? Parent { get; set; }

    SymbolTable? SymbolTable { get; set; }
}