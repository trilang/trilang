namespace Trilang.Semantics.Model;

public class GoTo : IStatement
{
    public GoTo(string label)
        => Label = label;

    public void Accept(IVisitor visitor)
        => visitor.VisitGoTo(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitGoTo(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformGoTo(this);

    public string Label { get; }

    public ISemanticNode? Parent { get; set; }
}