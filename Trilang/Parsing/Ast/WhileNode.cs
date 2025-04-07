using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class WhileNode : IStatementNode, IEquatable<WhileNode>
{
    public WhileNode(IExpressionNode condition, BlockStatementNode body)
    {
        Condition = condition;
        Body = body;

        Condition.Parent = this;
        Body.Parent = this;
    }

    public static bool operator ==(WhileNode? left, WhileNode? right)
        => Equals(left, right);

    public static bool operator !=(WhileNode? left, WhileNode? right)
        => !Equals(left, right);

    public bool Equals(WhileNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Condition.Equals(other.Condition) &&
               Body.Equals(other.Body) &&
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

        return Equals((WhileNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Condition, Body);

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

    public ISymbolTable? SymbolTable { get; set; }

    public IExpressionNode Condition { get; }

    public BlockStatementNode Body { get; }
}