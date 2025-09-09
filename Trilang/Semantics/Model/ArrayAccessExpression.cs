using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class ArrayAccessExpression : IExpression
{
    public ArrayAccessExpression(IExpression member, IExpression index)
    {
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
        => new ArrayAccessExpression(Member.Clone(), Index.Clone())
        {
            ReturnTypeMetadata = ReturnTypeMetadata,
        };

    public ISemanticNode? Parent { get; set; }

    public IExpression Member { get; }

    public IExpression Index { get; }

    public ITypeMetadata? ReturnTypeMetadata { get; set; }
}