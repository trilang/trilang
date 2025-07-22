using Trilang.Metadata;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class TupleTypeNode : IInlineTypeNode, IEquatable<TupleTypeNode>
{
    public TupleTypeNode(IReadOnlyList<IInlineTypeNode> types)
    {
        if (types.Count <= 1)
            throw new ArgumentException("Tuple must have at least 2 elements", nameof(types));

        Name = $"({string.Join(", ", types.Select(t => t.Name))})";
        Types = types;
    }

    public static bool operator ==(TupleTypeNode? left, TupleTypeNode? right)
        => Equals(left, right);

    public static bool operator !=(TupleTypeNode? left, TupleTypeNode? right)
        => !Equals(left, right);

    public bool Equals(TupleTypeNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Types.SequenceEqual(other.Types) &&
               Equals(Metadata, other.Metadata);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((TupleTypeNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name, Types);

    public void Accept(IVisitor visitor)
        => visitor.VisitTupleType(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitTupleType(this, context);

    public ISyntaxNode Transform(ITransformer transformer)
        => transformer.TransformTupleType(this);

    public ISyntaxNode? Parent { get; set; }

    public ISymbolTable? SymbolTable { get; set; }

    public string Name { get; }

    public IReadOnlyList<IInlineTypeNode> Types { get; }

    public ITypeMetadata? Metadata { get; set; }
}