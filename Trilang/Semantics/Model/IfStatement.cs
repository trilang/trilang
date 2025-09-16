namespace Trilang.Semantics.Model;

public class IfStatement : IStatement
{
    public IfStatement(
        SourceSpan? sourceSpan,
        IExpression condition,
        BlockStatement then,
        BlockStatement? @else = null)
    {
        SourceSpan = sourceSpan;
        Condition = condition;
        Then = then;
        Else = @else;

        Condition.Parent = this;
        Then.Parent = this;

        if (Else is not null)
            Else.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitIf(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitIf(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformIf(this);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public IExpression Condition { get; }

    public BlockStatement Then { get; }

    public BlockStatement? Else { get; }
}