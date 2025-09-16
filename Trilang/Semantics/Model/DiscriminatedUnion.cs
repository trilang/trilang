using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class DiscriminatedUnion : IInlineType
{
    public DiscriminatedUnion(SourceSpan? sourceSpan, IReadOnlyList<IInlineType> types)
    {
        SourceSpan = sourceSpan;
        Name = string.Join(" | ", types.Select(t => t.Name));
        Types = types;

        foreach (var type in Types)
            type.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitDiscriminatedUnion(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitDiscriminatedUnion(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformDiscriminatedUnion(this);

    public IInlineType Clone()
        => new DiscriminatedUnion(SourceSpan, Types.Select(t => t.Clone()).ToArray())
        {
            Metadata = Metadata,
        };

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public string Name { get; }

    public IReadOnlyList<IInlineType> Types { get; }

    public ITypeMetadata? Metadata { get; set; }
}