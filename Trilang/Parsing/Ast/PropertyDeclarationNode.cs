using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

// TODO: support getter-only/setter-only properties
// TODO: don't generate backing field if it's not needed
public class PropertyDeclarationNode : ISyntaxNode, IEquatable<PropertyDeclarationNode>
{
    private PropertyGetterNode? getter;
    private PropertySetterNode? setter;

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
               Equals(Setter, other.Setter) &&
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
        => visitor.VisitProperty(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitProperty(this, context);

    public ISyntaxNode Transform(ITransformer transformer)
        => transformer.TransformProperty(this);

    public ISyntaxNode? Parent { get; set; }

    public ISymbolTable? SymbolTable { get; set; }

    public string Name { get; }

    public IInlineTypeNode Type { get; }

    public PropertyGetterNode? Getter
    {
        get => getter;
        set
        {
            getter = value;

            if (getter is not null)
                getter.Parent = this;
        }
    }

    public PropertySetterNode? Setter
    {
        get => setter;
        set
        {
            setter = value;

            if (setter is not null)
                setter.Parent = this;
        }
    }

    public PropertyMetadata? Metadata { get; set; }
}