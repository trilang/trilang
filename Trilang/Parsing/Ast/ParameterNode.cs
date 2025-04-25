using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class ParameterNode : ISyntaxNode, IEquatable<ParameterNode>
{
    public ParameterNode(string name, IInlineTypeNode type)
    {
        Name = name;
        Type = type;
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

    public void Accept(IVisitor visitor)
        => visitor.Visit(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.Visit(this, context);

    public ISyntaxNode? Parent { get; set; }

    public ISymbolTable? SymbolTable { get; set; }

    public string Name { get; }

    public IInlineTypeNode Type { get; }
}