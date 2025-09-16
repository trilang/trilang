using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class TupleExpression : IExpression
{
    public TupleExpression(SourceSpan? sourceSpan, IReadOnlyList<IExpression> expressions)
    {
        if (expressions.Count <= 1)
            throw new ArgumentException("Tuple must have at least 2 elements", nameof(expressions));

        SourceSpan = sourceSpan;
        Expressions = expressions;

        foreach (var expression in expressions)
            expression.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitTuple(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitTuple(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformTuple(this);

    public IExpression Clone()
        => new TupleExpression(SourceSpan, Expressions.Select(x => x.Clone()).ToArray())
        {
            ReturnTypeMetadata = ReturnTypeMetadata,
        };

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public IReadOnlyList<IExpression> Expressions { get; }

    public ITypeMetadata? ReturnTypeMetadata { get; set; }
}