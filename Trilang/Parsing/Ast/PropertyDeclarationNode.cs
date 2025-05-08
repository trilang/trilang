using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class PropertyDeclarationNode : ISyntaxNode, IEquatable<PropertyDeclarationNode>
{
    public PropertyDeclarationNode(string name, IInlineTypeNode type)
        : this(name, type, null, null)
    {
    }

    public PropertyDeclarationNode(
        string name,
        IInlineTypeNode type,
        PropertyGetterNode? getter,
        PropertySetterNode? setter)
    {
        Name = name;
        Type = type;
        Getter = getter;
        Setter = setter;

        Type.Parent = this;

        if (Getter != null)
            Getter.Parent = this;

        if (Setter != null)
            Setter.Parent = this;
    }

    public static bool operator ==(PropertyDeclarationNode? left, PropertyDeclarationNode? right)
        => Equals(left, right);

    public static bool operator !=(PropertyDeclarationNode? left, PropertyDeclarationNode? right)
        => !Equals(left, right);

    public bool Equals(PropertyDeclarationNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name &&
               Type.Equals(other.Type) &&
               Equals(Getter, other.Getter) &&
               Equals(Setter, other.Setter);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((PropertyDeclarationNode)obj);
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

    public PropertyGetterNode? Getter { get; }

    public PropertySetterNode? Setter { get; }

    public PropertyMetadata? Metadata { get; set; }
}