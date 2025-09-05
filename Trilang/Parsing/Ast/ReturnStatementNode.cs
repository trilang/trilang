using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class ReturnStatementNode : IStatementNode, IEquatable<ReturnStatementNode>
{
    public ReturnStatementNode(IExpressionNode? expression = null)
    {
        Expression = expression;
        if (Expression is not null)
            Expression.Parent = this;
    }

    public static bool operator ==(ReturnStatementNode? left, ReturnStatementNode? right)
        => Equals(left, right);

    public static bool operator !=(ReturnStatementNode? left, ReturnStatementNode? right)
        => !Equals(left, right);

    public bool Equals(ReturnStatementNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Equals(Expression, other.Expression);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((ReturnStatementNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Expression);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitReturn(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitReturn(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformReturn(this);

    public ISyntaxNode? Parent { get; set; }

    public IExpressionNode? Expression { get; }
}