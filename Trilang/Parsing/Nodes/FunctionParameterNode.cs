using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Nodes;

public class FunctionParameterNode : ISyntaxNode, IEquatable<FunctionParameterNode>
{
    public FunctionParameterNode(string name, string type)
    {
        Name = name;
        Type = type;
    }

    public static bool operator ==(FunctionParameterNode? left, FunctionParameterNode? right)
        => Equals(left, right);

    public static bool operator !=(FunctionParameterNode? left, FunctionParameterNode? right)
        => !Equals(left, right);

    public bool Equals(FunctionParameterNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name && Type == other.Type;
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((FunctionParameterNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name, Type);

    public override string? ToString()
    {
        var formatter = new CommonFormatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.Visit(this);

    public string Name { get; }

    public string Type { get; }
}