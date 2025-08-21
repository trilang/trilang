using Trilang.Metadata;
using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

// TODO: replace by pattern matching
public class IsExpressionNode : IExpressionNode, IEquatable<IsExpressionNode>
{
    public IsExpressionNode(IExpressionNode expression, IInlineTypeNode type)
    {
        Expression = expression;
        Type = type;
    }

    public static bool operator ==(IsExpressionNode? left, IsExpressionNode? right)
        => Equals(left, right);

    public static bool operator !=(IsExpressionNode? left, IsExpressionNode? right)
        => !Equals(left, right);

    public bool Equals(IsExpressionNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Expression.Equals(other.Expression) &&
               Type.Equals(other.Type);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((IsExpressionNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Expression, Type);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitAsExpression(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitAsExpression(this, context);

    public ISyntaxNode Transform(ITransformer transformer)
        => transformer.TransformAsExpression(this);

    public IExpressionNode Clone()
        => new IsExpressionNode(Expression.Clone(), Type.Clone());

    public ISyntaxNode? Parent { get; set; }

    public IExpressionNode Expression { get; }

    public IInlineTypeNode Type { get; }

    public ITypeMetadata ReturnTypeMetadata
        => TypeMetadata.Bool;
}