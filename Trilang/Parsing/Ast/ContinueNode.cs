using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class ContinueNode : IStatementNode, IEquatable<ContinueNode>
{
    public static bool operator ==(ContinueNode? left, ContinueNode? right)
        => Equals(left, right);

    public static bool operator !=(ContinueNode? left, ContinueNode? right)
        => !Equals(left, right);

    public bool Equals(ContinueNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Equals(SymbolTable, other.SymbolTable);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((ContinueNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(this);

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

    public ISymbolTable? SymbolTable { get; set; }
}