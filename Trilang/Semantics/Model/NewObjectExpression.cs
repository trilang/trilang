using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class NewObjectExpression : IExpression
{
    public NewObjectExpression(SourceSpan? sourceSpan, IAccessExpression member)
    {
        SourceSpan = sourceSpan;
        Member = member;
        Member.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitNewObject(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitNewObject(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformNewObject(this);

    public IExpression Clone()
        => new NewObjectExpression(SourceSpan, (IAccessExpression)Member.Clone())
        {
            Metadata = Metadata,
        };

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public IAccessExpression Member { get; }

    public ConstructorMetadata? Metadata { get; set; }

    public ITypeMetadata? ReturnTypeMetadata { get; set; }
}