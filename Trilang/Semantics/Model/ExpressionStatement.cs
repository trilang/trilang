namespace Trilang.Semantics.Model;

public class ExpressionStatement : IStatement
{
    public ExpressionStatement(SourceSpan? sourceSpan, IExpression expression)
    {
        SourceSpan = sourceSpan;
        Expression = expression;
        Expression.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitExpressionStatement(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitExpressionStatement(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformExpressionStatement(this);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public IExpression Expression { get; }
}