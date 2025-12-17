using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class TypeRef : IInlineType
{
    public TypeRef(SourceSpan? sourceSpan, string name)
    {
        SourceSpan = sourceSpan;
        Name = name;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitTypeNode(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitTypeNode(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformTypeNode(this);

    public IInlineType Clone()
        => new TypeRef(SourceSpan, Name)
        {
            Metadata = Metadata,
        };

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public string Name { get; }

    public ITypeMetadata? Metadata { get; set; }
}