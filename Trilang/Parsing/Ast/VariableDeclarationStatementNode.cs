using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class VariableDeclarationStatementNode :
    VariableDeclarationNode,
    IStatementNode,
    IEquatable<VariableDeclarationStatementNode>
{
    public VariableDeclarationStatementNode(string name, IInlineTypeNode type, IExpressionNode expression)
        : base(name, type)
    {
        Expression = expression;
        Expression.Parent = this;
    }

    public static bool operator ==(VariableDeclarationStatementNode? left, VariableDeclarationStatementNode? right)
        => Equals(left, right);

    public static bool operator !=(VariableDeclarationStatementNode? left, VariableDeclarationStatementNode? right)
        => !Equals(left, right);

    public bool Equals(VariableDeclarationStatementNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return base.Equals(other) &&
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

        return Equals((VariableDeclarationStatementNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(base.GetHashCode(), Expression);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public override void Accept(IVisitor visitor)
        => visitor.Visit(this);

    public override void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.Visit(this, context);

    public IExpressionNode Expression { get; }
}