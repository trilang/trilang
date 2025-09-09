using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class Type : IInlineType
{
    public Type(string name)
        => Name = name;

    public void Accept(IVisitor visitor)
        => visitor.VisitTypeNode(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitTypeNode(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformTypeNode(this);

    public IInlineType Clone()
        => new Type(Name)
        {
            Metadata = Metadata,
        };

    public ISemanticNode? Parent { get; set; }

    public string Name { get; }

    public ITypeMetadata? Metadata { get; set; }
}