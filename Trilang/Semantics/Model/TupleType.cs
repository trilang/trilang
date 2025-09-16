using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class TupleType : IInlineType
{
    public TupleType(SourceSpan? sourceSpan, IReadOnlyList<IInlineType> types)
    {
        if (types.Count <= 1)
            throw new ArgumentException("Tuple must have at least 2 elements", nameof(types));

        SourceSpan = sourceSpan;
        Name = $"({string.Join(", ", types.Select(t => t.Name))})";
        Types = types;

        foreach (var type in Types)
            type.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitTupleType(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitTupleType(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformTupleType(this);

    public IInlineType Clone()
        => new TupleType(SourceSpan, Types.Select(t => t.Clone()).ToArray())
        {
            Metadata = Metadata,
        };

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public string Name { get; }

    public IReadOnlyList<IInlineType> Types { get; }

    public ITypeMetadata? Metadata { get; set; }
}