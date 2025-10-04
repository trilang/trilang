namespace Trilang.Semantics.Model;

public class FakeStatement : IStatement
{
    public FakeStatement(SourceSpan? sourceSpan)
        => SourceSpan = sourceSpan;

    public void Accept(IVisitor visitor)
        => visitor.VisitFakeStatement(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitFakeStatement(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformFakeStatement(this);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }
}