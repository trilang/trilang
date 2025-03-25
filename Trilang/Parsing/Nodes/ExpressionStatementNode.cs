using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Nodes;

public class ExpressionStatementNode : IStatementNode, IEquatable<ExpressionStatementNode>
{
    public ExpressionStatementNode(IExpressionNode expression)
        => Expression = expression;

    public static bool operator ==(ExpressionStatementNode? left, ExpressionStatementNode? right)
        => Equals(left, right);

    public static bool operator !=(ExpressionStatementNode? left, ExpressionStatementNode? right)
        => !Equals(left, right);

    public bool Equals(ExpressionStatementNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Expression.Equals(other.Expression);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((ExpressionStatementNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Expression);

    public override string? ToString()
    {
        var formatter = new CommonFormatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.Visit(this);

    public IExpressionNode Expression { get; }
}