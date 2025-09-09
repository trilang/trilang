using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class InterfaceProperty : ISemanticNode
{
    public InterfaceProperty(
        string name,
        IInlineType type,
        AccessModifier? getterModifier,
        AccessModifier? setterModifier)
    {
        Name = name;
        Type = type;
        GetterModifier = getterModifier;
        SetterModifier = setterModifier;

        Type.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitInterfaceProperty(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitInterfaceProperty(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformInterfaceProperty(this);

    public InterfaceProperty Clone()
        => new InterfaceProperty(Name, Type.Clone(), GetterModifier, SetterModifier)
        {
            Metadata = Metadata,
        };

    public ISemanticNode? Parent { get; set; }

    public string Name { get; }

    public IInlineType Type { get; }

    public AccessModifier? GetterModifier { get; }

    public AccessModifier? SetterModifier { get; }

    public InterfacePropertyMetadata? Metadata { get; set; }
}