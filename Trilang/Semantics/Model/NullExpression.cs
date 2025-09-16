using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class NullExpression : IExpression
{
    public NullExpression(SourceSpan? sourceSpan)
        => SourceSpan = sourceSpan;

    public void Accept(IVisitor visitor)
        => visitor.VisitNull(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitNull(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformNull(this);

    public IExpression Clone()
        => new NullExpression(SourceSpan);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public ITypeMetadata ReturnTypeMetadata
        => TypeMetadata.Null;
}