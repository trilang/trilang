using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class TypeNode : IInlineTypeNode, IEquatable<TypeNode>
{
    public TypeNode(string name)
        => Name = name;

    public static bool operator ==(TypeNode? left, TypeNode? right)
        => Equals(left, right);

    public static bool operator !=(TypeNode? left, TypeNode? right)
        => !Equals(left, right);

    public bool Equals(TypeNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name;
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((TypeNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitTypeNode(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitTypeNode(this, context);

    public ISyntaxNode Transform(ITransformer transformer)
        => transformer.TransformTypeNode(this);

    public ISyntaxNode? Parent { get; set; }

    public ISymbolTable? SymbolTable { get; set; }

    public string Name { get; }

    public ITypeMetadata? Metadata { get; set; }
}