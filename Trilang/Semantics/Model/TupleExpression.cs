using Trilang.Metadata;
using Trilang.Semantics.Providers;
using Trilang.Symbols;

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
            SymbolTable = SymbolTable,
            MetadataProvider = MetadataProvider,
        };

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public SymbolTable? SymbolTable { get; set; }

    public IMetadataProvider? MetadataProvider { get; set; }

    public IReadOnlyList<IExpression> Expressions { get; }

    public ITypeMetadata? ReturnTypeMetadata { get; set; }
}