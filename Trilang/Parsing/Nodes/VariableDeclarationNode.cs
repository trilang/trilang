using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Nodes;

public class VariableDeclarationNode : IStatementNode, IEquatable<VariableDeclarationNode>
{
    public VariableDeclarationNode(string name, string type, IExpressionNode expression)
    {
        Name = name;
        Type = type;
        Expression = expression;
    }

    public static bool operator ==(VariableDeclarationNode? left, VariableDeclarationNode? right)
        => Equals(left, right);

    public static bool operator !=(VariableDeclarationNode? left, VariableDeclarationNode? right)
        => !Equals(left, right);

    public bool Equals(VariableDeclarationNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name &&
               Type == other.Type &&
               Expression.Equals(other.Expression);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((VariableDeclarationNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name, Type, Expression);

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

    public IExpressionNode Expression { get; }
}