using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

// TODO: replace by pattern matching
public class AsExpressionNode : IExpressionNode, IEquatable<AsExpressionNode>
{
    public AsExpressionNode(IExpressionNode expression, IInlineTypeNode type)
    {
        Expression = expression;
        Type = type;
    }

    public static bool operator ==(AsExpressionNode? left, AsExpressionNode? right)
        => Equals(left, right);

    public static bool operator !=(AsExpressionNode? left, AsExpressionNode? right)
        => !Equals(left, right);

    public bool Equals(AsExpressionNode? other)
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

        return Equals((AsExpressionNode)obj);
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

    public ISyntaxNode? Parent { get; set; }

    public ISymbolTable? SymbolTable { get; set; }

    public IExpressionNode Expression { get; }

    public IInlineTypeNode Type { get; }

    public ITypeMetadata? ReturnTypeMetadata { get; set; }
}