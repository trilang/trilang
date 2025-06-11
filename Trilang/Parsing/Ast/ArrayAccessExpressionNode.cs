using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class ArrayAccessExpressionNode : IExpressionNode, IEquatable<ArrayAccessExpressionNode>
{
    public ArrayAccessExpressionNode(IExpressionNode member, IExpressionNode index)
    {
        Member = member;
        Index = index;

        Member.Parent = this;
        Index.Parent = this;
    }

    public static bool operator ==(ArrayAccessExpressionNode? left, ArrayAccessExpressionNode? right)
        => Equals(left, right);

    public static bool operator !=(ArrayAccessExpressionNode? left, ArrayAccessExpressionNode? right)
        => !Equals(left, right);

    public bool Equals(ArrayAccessExpressionNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Member.Equals(other.Member) &&
               Index.Equals(other.Index);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((ArrayAccessExpressionNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Member);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitArrayAccess(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitArrayAccess(this, context);

    public ISyntaxNode? Parent { get; set; }

    public ISymbolTable? SymbolTable { get; set; }

    public IExpressionNode Member { get; }

    public IExpressionNode Index { get; }

    public ITypeMetadata? ReturnTypeMetadata { get; set; }
}