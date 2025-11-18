namespace Trilang.Semantics.Model;

public class Namespace : ISemanticNode
{
    public Namespace(SourceSpan? sourceSpan, IReadOnlyList<string> parts)
    {
        SourceSpan = sourceSpan;
        Parts = parts;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitNamespace(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitNamespace(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformNamespace(this);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public IReadOnlyList<string> Parts { get; }
}