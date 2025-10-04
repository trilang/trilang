namespace Trilang.Semantics.Model;

public class FakeDeclaration : IDeclaration
{
    public FakeDeclaration(SourceSpan? sourceSpan)
        => SourceSpan = sourceSpan;

    public void Accept(IVisitor visitor)
        => visitor.VisitFakeDeclaration(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitFakeDeclaration(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformFakeDeclaration(this);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }
}
