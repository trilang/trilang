using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Nodes;

public class UnaryExpressionNode : IExpressionNode, IEquatable<UnaryExpressionNode>
{
    public UnaryExpressionNode(UnaryExpressionKind kind, IExpressionNode operand)
    {
        Kind = kind;
        Operand = operand;
    }

    public static bool operator ==(UnaryExpressionNode? left, UnaryExpressionNode? right)
        => Equals(left, right);

    public static bool operator !=(UnaryExpressionNode? left, UnaryExpressionNode? right)
        => !Equals(left, right);

    public bool Equals(UnaryExpressionNode? other)
    {
        if (other is null) return false;

        if (ReferenceEquals(this, other))
            return true;

        return Kind.Equals(other.Kind) && Operand.Equals(other.Operand);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null) return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((UnaryExpressionNode)obj);
    }

    public override int GetHashCode()
        => Operand.GetHashCode();

    public override string? ToString()
    {
        var formatter = new CommonFormatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.Visit(this);

    public UnaryExpressionKind Kind { get; }

    public IExpressionNode Operand { get; }
}