using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Nodes;

public class BlockStatementNode : IStatementNode, IEquatable<BlockStatementNode>
{
    public BlockStatementNode()
        : this([])
    {
    }

    public BlockStatementNode(IReadOnlyList<IStatementNode> statements)
        => Statements = statements;

    public static bool operator ==(BlockStatementNode? left, BlockStatementNode? right)
        => Equals(left, right);

    public static bool operator !=(BlockStatementNode? left, BlockStatementNode? right)
        => !Equals(left, right);

    public bool Equals(BlockStatementNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Statements.SequenceEqual(other.Statements);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((BlockStatementNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Statements);

    public override string? ToString()
    {
        var formatter = new CommonFormatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.Visit(this);

    public IReadOnlyList<IStatementNode> Statements { get; }
}