using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class ParameterNode : VariableDeclarationNode, IEquatable<ParameterNode>
{
    public ParameterNode(string name, IInlineTypeNode type)
        : base(name, type)
    {
    }

    public static bool operator ==(ParameterNode? left, ParameterNode? right)
        => Equals(left, right);

    public static bool operator !=(ParameterNode? left, ParameterNode? right)
        => !Equals(left, right);

    public bool Equals(ParameterNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name &&
               Type.Equals(other.Type);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((ParameterNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name, Type);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public override void Accept(IVisitor visitor)
        => visitor.Visit(this);

    public override void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.Visit(this, context);
}