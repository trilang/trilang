using Trilang.Metadata;
using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class TypeAliasDeclarationNode :
    IDeclarationNode,
    IHasGenericArguments,
    IEquatable<TypeAliasDeclarationNode>
{
    public TypeAliasDeclarationNode(
        AccessModifier accessModifier,
        string name,
        IReadOnlyList<TypeNode> genericArguments,
        IInlineTypeNode type)
    {
        AccessModifier = accessModifier;
        Name = name;
        GenericArguments = genericArguments;
        Type = type;

        foreach (var genericArgument in genericArguments)
            genericArgument.Parent = this;

        Type.Parent = this;

        FullName = genericArguments.Count > 0
            ? $"{Name}<{new string(',', genericArguments.Count - 1)}>"
            : Name;
    }

    public static bool operator ==(TypeAliasDeclarationNode? left, TypeAliasDeclarationNode? right)
        => Equals(left, right);

    public static bool operator !=(TypeAliasDeclarationNode? left, TypeAliasDeclarationNode? right)
        => !Equals(left, right);

    public bool Equals(TypeAliasDeclarationNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return AccessModifier == other.AccessModifier &&
               Name == other.Name &&
               GenericArguments.SequenceEqual(other.GenericArguments) &&
               Type.Equals(other.Type) &&
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

        return Equals((TypeAliasDeclarationNode)obj);
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
        => visitor.VisitTypeAlias(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitTypeAlias(this, context);

    public ISyntaxNode Transform(ITransformer transformer)
        => transformer.TransformTypeAlias(this);

    public ISyntaxNode? Parent { get; set; }

    public AccessModifier AccessModifier { get; }

    public string Name { get; }

    public string FullName { get; }

    public IReadOnlyList<TypeNode> GenericArguments { get; }

    public IInlineTypeNode Type { get; }

    public ITypeMetadata? Metadata { get; set; }
}