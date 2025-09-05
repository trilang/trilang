using Trilang.Metadata;
using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class InterfacePropertyNode : ISyntaxNode, IEquatable<InterfacePropertyNode>
{
    public InterfacePropertyNode(
        string name,
        IInlineTypeNode type,
        AccessModifier? getterModifier,
        AccessModifier? setterModifier)
    {
        Name = name;
        Type = type;
        GetterModifier = getterModifier;
        SetterModifier = setterModifier;

        Type.Parent = this;
    }

    public static bool operator ==(InterfacePropertyNode? left, InterfacePropertyNode? right)
        => Equals(left, right);

    public static bool operator !=(InterfacePropertyNode? left, InterfacePropertyNode? right)
        => !Equals(left, right);

    public bool Equals(InterfacePropertyNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name &&
               Type.Equals(other.Type) &&
               GetterModifier == other.GetterModifier &&
               SetterModifier == other.SetterModifier &&
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

        return Equals((InterfacePropertyNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name, Type, GetterModifier, SetterModifier);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitInterfaceProperty(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitInterfaceProperty(this, context);

    public ISyntaxNode Transform(ITransformer transformer)
        => transformer.TransformInterfaceProperty(this);

    public InterfacePropertyNode Clone()
        => new InterfacePropertyNode(Name, Type.Clone(), GetterModifier, SetterModifier)
        {
            Metadata = Metadata,
        };

    public ISyntaxNode? Parent { get; set; }

    public string Name { get; }

    public IInlineTypeNode Type { get; }

    public AccessModifier? GetterModifier { get; }

    public AccessModifier? SetterModifier { get; }

    public InterfacePropertyMetadata? Metadata { get; set; }
}