using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class PointerType : IInlineType
{
    public PointerType(SourceSpan? sourceSpan, IInlineType type)
    {
        SourceSpan = sourceSpan;
        Name = $"{type.Name}*";
        Type = type;
        Type.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitPointer(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitPointer(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformPointer(this);

    public IInlineType Clone()
        => new PointerType(SourceSpan, Type.Clone())
        {
            Metadata = Metadata,
        };

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public string Name { get; }

    public ITypeMetadata? Metadata { get; set; }

    public IInlineType Type { get; }
}