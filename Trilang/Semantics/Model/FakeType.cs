using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class FakeType : IInlineType
{
    public FakeType(SourceSpan? sourceSpan, string name)
    {
        SourceSpan = sourceSpan;
        Name = name;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitFakeType(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitFakeType(this, context);

    public IInlineType Clone()
        => new FakeType(SourceSpan, Name);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformFakeType(this);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public string Name { get; }

    public ITypeMetadata? Metadata => TypeMetadata.InvalidType;
}