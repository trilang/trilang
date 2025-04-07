using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class IfStatementNode : IStatementNode, IEquatable<IfStatementNode>
{
    public IfStatementNode(IExpressionNode condition, BlockStatementNode then, BlockStatementNode? @else = null)
    {
        Condition = condition;
        Then = then;
        Else = @else;

        Condition.Parent = this;
        Then.Parent = this;

        if (Else is not null)
            Else.Parent = this;
    }

    public static bool operator ==(IfStatementNode? left, IfStatementNode? right)
        => Equals(left, right);

    public static bool operator !=(IfStatementNode? left, IfStatementNode? right)
        => !Equals(left, right);

    public bool Equals(IfStatementNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Condition.Equals(other.Condition) &&
               Then.Equals(other.Then) &&
               Equals(Else, other.Else) &&
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

        return Equals((IfStatementNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Condition, Then, Else);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.Visit(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.Visit(this, context);

    public ISyntaxNode? Parent { get; set; }

    public IExpressionNode Condition { get; }

    public BlockStatementNode Then { get; }

    public BlockStatementNode? Else { get; }

    public ISymbolTable? SymbolTable { get; set; }
}