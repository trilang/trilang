using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class FakeExpression : IExpression
{
    public FakeExpression(SourceSpan? sourceSpan)
        => SourceSpan = sourceSpan;

    public void Accept(IVisitor visitor)
        => visitor.VisitFakeExpression(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitFakeExpression(this, context);

    public IExpression Clone()
        => new FakeExpression(SourceSpan);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformFakeExpression(this);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public ITypeMetadata? ReturnTypeMetadata => TypeMetadata.InvalidType;
}