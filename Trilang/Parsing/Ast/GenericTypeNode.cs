using Trilang.Metadata;
using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class GenericTypeNode : IInlineTypeNode, IEquatable<GenericTypeNode>
{
    public GenericTypeNode(string prefixName, IReadOnlyList<IInlineTypeNode> typeArguments)
    {
        PrefixName = prefixName;
        TypeArguments = typeArguments;
        Name = $"{prefixName}<{string.Join(", ", typeArguments.Select(t => t.Name))}>";

        foreach (var typeArgument in TypeArguments)
            typeArgument.Parent = this;
    }

    public static bool operator ==(GenericTypeNode? left, GenericTypeNode? right)
        => Equals(left, right);

    public static bool operator !=(GenericTypeNode? left, GenericTypeNode? right)
        => !Equals(left, right);

    public bool Equals(GenericTypeNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name &&
               TypeArguments.SequenceEqual(other.TypeArguments) &&
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

        return Equals((GenericTypeNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name, TypeArguments);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitGenericType(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitGenericType(this, context);

    public ISyntaxNode Transform(ITransformer transformer)
        => transformer.TransformGenericType(this);

    public IInlineTypeNode Clone()
        => new GenericTypeNode(PrefixName, TypeArguments.Select(t => t.Clone()).ToArray())
        {
            Metadata = Metadata,
        };

    public string GetOpenGenericName()
        => $"{PrefixName}<{new string(',', TypeArguments.Count - 1)}>";

    public ISyntaxNode? Parent { get; set; }

    public string PrefixName { get; }

    public IReadOnlyList<IInlineTypeNode> TypeArguments { get; }

    public string Name { get; }

    public ITypeMetadata? Metadata { get; set; }
}