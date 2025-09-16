using Trilang.Metadata;

namespace Trilang.Semantics.Model;

// TODO: replace by pattern matching
public class IsExpression : IExpression
{
    public IsExpression(SourceSpan? sourceSpan, IExpression expression, IInlineType type)
    {
        SourceSpan = sourceSpan;
        Expression = expression;
        Type = type;

        Expression.Parent = this;
        Type.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitIsExpression(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitIsExpression(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformAsExpression(this);

    public IExpression Clone()
        => new IsExpression(SourceSpan, Expression.Clone(), Type.Clone());

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public IExpression Expression { get; }

    public IInlineType Type { get; }

    public ITypeMetadata ReturnTypeMetadata
        => TypeMetadata.Bool;
}