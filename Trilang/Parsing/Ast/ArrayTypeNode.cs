using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class ArrayTypeNode : IInlineTypeNode, IEquatable<ArrayTypeNode>
{
    public ArrayTypeNode(IInlineTypeNode elementType)
    {
        Name = $"{elementType.Name}[]";
        ElementType = elementType;
        ElementType.Parent = this;
    }

    public static bool operator ==(ArrayTypeNode? left, ArrayTypeNode? right)
        => Equals(left, right);

    public static bool operator !=(ArrayTypeNode? left, ArrayTypeNode? right)
        => !Equals(left, right);

    public bool Equals(ArrayTypeNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return ElementType.Equals(other.ElementType);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((ArrayTypeNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(ElementType);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitArrayType(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitArrayType(this, context);

    public ISyntaxNode? Parent { get; set; }

    public ISymbolTable? SymbolTable { get; set; }

    public string Name { get; }

    public IInlineTypeNode ElementType { get; }

    public ITypeMetadata? Metadata { get; set; }
}