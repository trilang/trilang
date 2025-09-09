namespace Trilang.Semantics.Model;

public class ReturnStatement : IStatement
{
    public ReturnStatement(IExpression? expression = null)
    {
        Expression = expression;
        if (Expression is not null)
            Expression.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitReturn(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitReturn(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformReturn(this);

    public ISemanticNode? Parent { get; set; }

    public IExpression? Expression { get; }
}