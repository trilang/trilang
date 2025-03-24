using System.Diagnostics.CodeAnalysis;

namespace Trilang.Parsing.Nodes;

public class FunctionStatementNode : IStatementNode, IEquatable<FunctionStatementNode>
{
    private FunctionStatementNode(
        bool isExternal,
        string name,
        IReadOnlyList<FunctionParameterNode> parameters,
        string returnType,
        BlockStatementNode? body)
    {
        IsExternal = isExternal;
        Name = name;
        Parameters = parameters;
        ReturnType = returnType;
        Body = body;
    }

    public static FunctionStatementNode Create(
        string name,
        IReadOnlyList<FunctionParameterNode> parameters,
        string returnType,
        BlockStatementNode body)
        => new FunctionStatementNode(false, name, parameters, returnType, body);

    public static FunctionStatementNode CreateExternal(
        string name,
        IReadOnlyList<FunctionParameterNode> parameters,
        string returnType)
        => new FunctionStatementNode(true, name, parameters, returnType, null);

    public static bool operator ==(FunctionStatementNode? left, FunctionStatementNode? right)
        => Equals(left, right);

    public static bool operator !=(FunctionStatementNode? left, FunctionStatementNode? right)
        => !Equals(left, right);

    public bool Equals(FunctionStatementNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return IsExternal == other.IsExternal &&
               Name == other.Name &&
               Parameters.SequenceEqual(other.Parameters) &&
               ReturnType == other.ReturnType &&
               Equals(Body, other.Body);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((FunctionStatementNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(IsExternal, Name, Parameters, ReturnType, Body);

    [MemberNotNullWhen(false, nameof(Body))]
    public bool IsExternal { get; }

    public string Name { get; }

    public IReadOnlyList<FunctionParameterNode> Parameters { get; }

    public string ReturnType { get; }

    public BlockStatementNode? Body { get; }
}