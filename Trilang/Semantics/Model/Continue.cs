namespace Trilang.Semantics.Model;

public class Continue : IStatement
{
    public void Accept(IVisitor visitor)
        => visitor.VisitContinue(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitContinue(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformContinue(this);

    public ISemanticNode? Parent { get; set; }

    public While? LoopNode { get; set; }
}