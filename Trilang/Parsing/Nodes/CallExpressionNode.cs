namespace Trilang.Parsing.Nodes;

public class CallExpressionNode : IExpressionNode, IEquatable<CallExpressionNode>
{
    public CallExpressionNode(string functionName, IReadOnlyList<IExpressionNode> parameters)
    {
        FunctionName = functionName;
        Parameters = parameters;
    }

    public static bool operator ==(CallExpressionNode? left, CallExpressionNode? right)
        => Equals(left, right);

    public static bool operator !=(CallExpressionNode? left, CallExpressionNode? right)
        => !Equals(left, right);

    public bool Equals(CallExpressionNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return FunctionName == other.FunctionName && Parameters.SequenceEqual(other.Parameters);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((CallExpressionNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(FunctionName, Parameters);

    public string FunctionName { get; }

    public IReadOnlyList<IExpressionNode> Parameters { get; }
}