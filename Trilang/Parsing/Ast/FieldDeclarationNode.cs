using Trilang.Metadata;
using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class FieldDeclarationNode : VariableDeclarationNode, IEquatable<FieldDeclarationNode>
{
    public FieldDeclarationNode(AccessModifier accessModifier, string name, IInlineTypeNode type)
        : base(name, type)
    {
        AccessModifier = accessModifier;
    }

    public static bool operator ==(FieldDeclarationNode? left, FieldDeclarationNode? right)
        => Equals(left, right);

    public static bool operator !=(FieldDeclarationNode? left, FieldDeclarationNode? right)
        => !Equals(left, right);

    public bool Equals(FieldDeclarationNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return AccessModifier == other.AccessModifier &&
               Equals(Metadata, other.Metadata) &&
               base.Equals(other);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((FieldDeclarationNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(base.GetHashCode(), (int)AccessModifier);

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

    public AccessModifier AccessModifier { get; }

    public FieldMetadata? Metadata { get; set; }
}