namespace Trilang.Semantics.Model;

public interface ISemanticNode
{
    void Accept(IVisitor visitor);

    void Accept<TContext>(IVisitor<TContext> visitor, TContext context);

    T Transform<T>(ITransformer<T> transformer);

    ISemanticNode? Parent { get; set; }
}