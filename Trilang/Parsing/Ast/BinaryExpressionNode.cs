using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class BinaryExpressionNode : IExpressionNode, IEquatable<BinaryExpressionNode>
{
    public BinaryExpressionNode(
        BinaryExpressionKind kind,
        IExpressionNode left,
        IExpressionNode right)
    {
        Kind = kind;
        Left = left;
        Right = right;

        Left.Parent = this;
        Right.Parent = this;
    }

    public static bool operator ==(BinaryExpressionNode? left, BinaryExpressionNode? right)
        => Equals(left, right);

    public static bool operator !=(BinaryExpressionNode? left, BinaryExpressionNode? right)
        => !Equals(left, right);

    public bool Equals(BinaryExpressionNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Kind.Equals(other.Kind) &&
               Equals(ReturnTypeMetadata, other.ReturnTypeMetadata) &&
               Left.Equals(other.Left) &&
               Right.Equals(other.Right);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((BinaryExpressionNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Left, Right);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitBinaryExpression(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitBinaryExpression(this, context);

    public ISyntaxNode? Parent { get; set; }

    public BinaryExpressionKind Kind { get; }

    public IExpressionNode Left { get; }

    public IExpressionNode Right { get; }

    public ITypeMetadata? ReturnTypeMetadata { get; set; }

    public ISymbolTable? SymbolTable { get; set; }
}