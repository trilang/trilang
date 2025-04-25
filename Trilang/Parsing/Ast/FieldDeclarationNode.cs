using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class FieldDeclarationNode : ISyntaxNode, IEquatable<FieldDeclarationNode>
{
    public FieldDeclarationNode(AccessModifier accessModifier, string name, IInlineTypeNode type)
    {
        AccessModifier = accessModifier;
        Name = name;
        Type = type;

        Type.Parent = this;
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
               Name == other.Name &&
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

        return Equals((FieldDeclarationNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine((int)AccessModifier, Name, Type);

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

    public AccessModifier AccessModifier { get; }

    public string Name { get; }

    public IInlineTypeNode Type { get; }

    public FieldMetadata? Metadata { get; set; }
}