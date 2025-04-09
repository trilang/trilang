using Trilang.Metadata;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class TypeAliasNode : IDeclarationNode, IEquatable<TypeAliasNode>
{
    public TypeAliasNode(AccessModifier accessModifier, string name, TypeNode type)
    {
        AccessModifier = accessModifier;
        Name = name;
        Type = type;
    }

    public static bool operator ==(TypeAliasNode? left, TypeAliasNode? right)
        => Equals(left, right);

    public static bool operator !=(TypeAliasNode? left, TypeAliasNode? right)
        => !Equals(left, right);

    public bool Equals(TypeAliasNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return AccessModifier == other.AccessModifier &&
               Name == other.Name &&
               Type.Equals(other.Type) &&
               Equals(SymbolTable, other.SymbolTable);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((TypeAliasNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine((int)AccessModifier, Name, Type);

    public void Accept(IVisitor visitor)
        => visitor.Visit(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.Visit(this, context);

    public ISyntaxNode? Parent { get; set; }

    public ISymbolTable? SymbolTable { get; set; }

    public AccessModifier AccessModifier { get; }

    public string Name { get; }

    public TypeNode Type { get; }

    public IMetadata? Metadata { get; set; }
}