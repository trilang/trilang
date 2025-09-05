using Trilang.Metadata;
using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class CastExpressionNode : IExpressionNode, IEquatable<CastExpressionNode>
{
    public CastExpressionNode(IInlineTypeNode type, IExpressionNode expression)
    {
        Type = type;
        Expression = expression;

        Type.Parent = this;
        Expression.Parent = this;
    }

    public static bool operator ==(CastExpressionNode? left, CastExpressionNode? right)
        => Equals(left, right);

    public static bool operator !=(CastExpressionNode? left, CastExpressionNode? right)
        => !Equals(left, right);

    public bool Equals(CastExpressionNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Type.Equals(other.Type) &&
               Expression.Equals(other.Expression);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((CastExpressionNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Type, Expression);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitCast(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitCast(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformCast(this);

    public IExpressionNode Clone()
        => new CastExpressionNode(Type.Clone(), Expression.Clone());

    public ISyntaxNode? Parent { get; set; }

    public ITypeMetadata? ReturnTypeMetadata
        => Type.Metadata;

    public IInlineTypeNode Type { get; }

    public IExpressionNode Expression { get; }
}