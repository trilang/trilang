namespace Trilang.Parsing.Nodes;

public class SyntaxTree : IEquatable<SyntaxTree>
{
    public SyntaxTree(IReadOnlyList<FunctionStatementNode> functions)
    {
        Functions = functions;
    }

    public static bool operator ==(SyntaxTree? left, SyntaxTree? right)
        => Equals(left, right);

    public static bool operator !=(SyntaxTree? left, SyntaxTree? right)
        => !Equals(left, right);

    public bool Equals(SyntaxTree? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Functions.SequenceEqual(other.Functions);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((SyntaxTree)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Functions);

    public IReadOnlyList<FunctionStatementNode> Functions { get; }
}