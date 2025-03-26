using System.Diagnostics.CodeAnalysis;
using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Nodes;

public class FunctionDeclarationNode : IStatementNode, IEquatable<FunctionDeclarationNode>
{
    private FunctionDeclarationNode(
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

    public static FunctionDeclarationNode Create(
        string name,
        IReadOnlyList<FunctionParameterNode> parameters,
        string returnType,
        BlockStatementNode body)
        => new FunctionDeclarationNode(false, name, parameters, returnType, body);

    public static FunctionDeclarationNode CreateExternal(
        string name,
        IReadOnlyList<FunctionParameterNode> parameters,
        string returnType)
        => new FunctionDeclarationNode(true, name, parameters, returnType, null);

    public static bool operator ==(FunctionDeclarationNode? left, FunctionDeclarationNode? right)
        => Equals(left, right);

    public static bool operator !=(FunctionDeclarationNode? left, FunctionDeclarationNode? right)
        => !Equals(left, right);

    public bool Equals(FunctionDeclarationNode? other)
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

        return Equals((FunctionDeclarationNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(IsExternal, Name, Parameters, ReturnType, Body);

    public override string? ToString()
    {
        var formatter = new CommonFormatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.Visit(this);

    [MemberNotNullWhen(false, nameof(Body))]
    public bool IsExternal { get; }

    public string Name { get; }

    public IReadOnlyList<FunctionParameterNode> Parameters { get; }

    public string ReturnType { get; }

    public BlockStatementNode? Body { get; }
}