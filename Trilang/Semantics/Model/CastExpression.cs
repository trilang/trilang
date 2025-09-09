using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class CastExpression : IExpression
{
    public CastExpression(IInlineType type, IExpression expression)
    {
        Type = type;
        Expression = expression;

        Type.Parent = this;
        Expression.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitCast(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitCast(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformCast(this);

    public IExpression Clone()
        => new CastExpression(Type.Clone(), Expression.Clone());

    public ISemanticNode? Parent { get; set; }

    public ITypeMetadata? ReturnTypeMetadata
        => Type.Metadata;

    public IInlineType Type { get; }

    public IExpression Expression { get; }
}