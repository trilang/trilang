namespace Trilang.Semantics.Model;

public class While : IStatement
{
    public While(SourceSpan? sourceSpan, IExpression condition, BlockStatement body)
    {
        SourceSpan = sourceSpan;
        Condition = condition;
        Body = body;

        Condition.Parent = this;
        Body.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitWhile(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitWhile(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformWhile(this);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public IExpression Condition { get; }

    public BlockStatement Body { get; }
}