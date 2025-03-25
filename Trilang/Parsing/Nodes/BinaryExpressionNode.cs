using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Nodes;

public class BinaryExpressionNode : IExpressionNode, IEquatable<BinaryExpressionNode>
{
    public BinaryExpressionNode(BinaryExpressionKind kind, IExpressionNode left, IExpressionNode right)
    {
        Kind = kind;
        Left = left;
        Right = right;
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

        return Kind.Equals(other.Kind) && Left.Equals(other.Left) && Right.Equals(other.Right);
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

    public override string? ToString()
    {
        var formatter = new CommonFormatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.Visit(this);

    public BinaryExpressionKind Kind { get; }

    public IExpressionNode Left { get; }

    public IExpressionNode Right { get; }
}