using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class ExpressionStatementNode : IStatementNode, IEquatable<ExpressionStatementNode>
{
    public ExpressionStatementNode(IExpressionNode expression)
    {
        Expression = expression;
        Expression.Parent = this;
    }

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

        return Expression.Equals(other.Expression) &&
               Equals(SymbolTable, other.SymbolTable);
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

    public override string ToString()
    {
        var formatter = new CommonFormatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.Visit(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.Visit(this, context);

    public ISyntaxNode? Parent { get; set; }

    public IExpressionNode Expression { get; }

    public SymbolTable? SymbolTable { get; set; }
}