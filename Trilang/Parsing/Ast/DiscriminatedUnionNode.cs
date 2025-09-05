using Trilang.Metadata;
using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class DiscriminatedUnionNode : IInlineTypeNode, IEquatable<DiscriminatedUnionNode>
{
    public DiscriminatedUnionNode(IReadOnlyList<IInlineTypeNode> types)
    {
        Name = string.Join(" | ", types.Select(t => t.Name));
        Types = types;

        foreach (var type in Types)
            type.Parent = this;
    }

    public static bool operator ==(DiscriminatedUnionNode? left, DiscriminatedUnionNode? right)
        => Equals(left, right);

    public static bool operator !=(DiscriminatedUnionNode? left, DiscriminatedUnionNode? right)
        => !Equals(left, right);

    public bool Equals(DiscriminatedUnionNode? other)
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

        return Equals((DiscriminatedUnionNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Types);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitDiscriminatedUnion(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitDiscriminatedUnion(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformDiscriminatedUnion(this);

    public IInlineTypeNode Clone()
        => new DiscriminatedUnionNode(Types.Select(t => t.Clone()).ToArray())
        {
            Metadata = Metadata,
        };

    public ISyntaxNode? Parent { get; set; }

    public string Name { get; }

    public IReadOnlyList<IInlineTypeNode> Types { get; }

    public ITypeMetadata? Metadata { get; set; }
}