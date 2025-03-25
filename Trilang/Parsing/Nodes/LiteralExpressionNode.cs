using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Nodes;

public class LiteralExpressionNode : IExpressionNode, IEquatable<LiteralExpressionNode>
{
    public LiteralExpressionNode(LiteralExpressionKind kind, object value)
    {
        Kind = kind;
        Value = value;
    }

    public static bool operator ==(LiteralExpressionNode? left, LiteralExpressionNode? right)
        => Equals(left, right);

    public static bool operator !=(LiteralExpressionNode? left, LiteralExpressionNode? right)
        => !Equals(left, right);

    public bool Equals(LiteralExpressionNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Kind == other.Kind && Value.Equals(other.Value);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((LiteralExpressionNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine((int)Kind, Value);

    public override string? ToString()
    {
        var formatter = new CommonFormatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.Visit(this);

    public LiteralExpressionKind Kind { get; }

    public object Value { get; }
}