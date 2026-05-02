namespace Trilang.Semantics.Model;

public class Use : ISemanticNode
{
    public Use(SourceSpan? sourceSpan, string? package, IReadOnlyList<string> parts)
    {
        SourceSpan = sourceSpan;
        Package = package;
        Parts = parts;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitUse(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitUse(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformUse(this);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public string? Package { get; }

    public IReadOnlyList<string> Parts { get; }
}