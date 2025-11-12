namespace Trilang.Semantics.Model;

public class ReturnStatement : IStatement
{
    public ReturnStatement(SourceSpan? sourceSpan, IExpression? expression = null)
    {
        SourceSpan = sourceSpan;
        Expression = expression;
        Expression?.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitReturn(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitReturn(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformReturn(this);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public IExpression? Expression { get; }
}