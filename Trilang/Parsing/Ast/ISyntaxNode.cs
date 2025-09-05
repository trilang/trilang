namespace Trilang.Parsing.Ast;

public interface ISyntaxNode
{
    void Accept(IVisitor visitor);

    void Accept<TContext>(IVisitor<TContext> visitor, TContext context);

    T Transform<T>(ITransformer<T> transformer);

    ISyntaxNode? Parent { get; set; }
}