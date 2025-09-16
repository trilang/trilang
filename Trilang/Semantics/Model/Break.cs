namespace Trilang.Semantics.Model;

public class Break : IStatement
{
    public Break(SourceSpan? sourceSpan)
        => SourceSpan = sourceSpan;

    public void Accept(IVisitor visitor)
        => visitor.VisitBreak(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitBreak(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformBreak(this);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public While? LoopNode { get; set; }
}