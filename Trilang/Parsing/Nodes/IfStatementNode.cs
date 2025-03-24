namespace Trilang.Parsing.Nodes;

public class IfStatementNode : IStatementNode, IEquatable<IfStatementNode>
{
    public IfStatementNode(IExpressionNode condition, BlockStatementNode then, BlockStatementNode? @else)
    {
        Condition = condition;
        Then = then;
        Else = @else;
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
               Equals(Else, other.Else);
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

    public IExpressionNode Condition { get; }

    public BlockStatementNode Then { get; }

    public BlockStatementNode? Else { get; }
}