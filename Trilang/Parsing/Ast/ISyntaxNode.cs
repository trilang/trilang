namespace Trilang.Parsing.Ast;

public interface ISyntaxNode
{
    void Accept(IVisitor visitor);

    void Accept<TContext>(IVisitor<TContext> visitor, TContext context);

    ISyntaxNode Transform(ITransformer transformer);

    ISyntaxNode? Parent { get; set; }
}