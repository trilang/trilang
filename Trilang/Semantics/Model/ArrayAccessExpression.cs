using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class ArrayAccessExpression : IExpression
{
    public ArrayAccessExpression(SourceSpan? sourceSpan, IExpression member, IExpression index)
    {
        SourceSpan = sourceSpan;
        Member = member;
        Index = index;

        Member.Parent = this;
        Index.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitArrayAccess(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitArrayAccess(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformArrayAccess(this);

    public IExpression Clone()
        => new ArrayAccessExpression(SourceSpan, Member.Clone(), Index.Clone())
        {
            ReturnTypeMetadata = ReturnTypeMetadata,
        };

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public IExpression Member { get; }

    public IExpression Index { get; }

    public ITypeMetadata? ReturnTypeMetadata { get; set; }
}