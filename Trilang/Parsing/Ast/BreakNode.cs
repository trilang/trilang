using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class BreakNode : IStatementNode, IEquatable<BreakNode>
{
    public static bool operator ==(BreakNode? left, BreakNode? right)
        => Equals(left, right);

    public static bool operator !=(BreakNode? left, BreakNode? right)
        => !Equals(left, right);

    public bool Equals(BreakNode? other)
    {
        if (other is null)
            return false;

        return true;
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((BreakNode)obj);
    }

    public override int GetHashCode()
        => 0;

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitBreak(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitBreak(this, context);

    public ISyntaxNode? Parent { get; set; }

    public ISymbolTable? SymbolTable { get; set; }

    public WhileNode? LoopNode { get; set; }
}