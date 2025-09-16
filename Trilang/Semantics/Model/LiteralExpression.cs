using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class LiteralExpression : IExpression
{
    public LiteralExpression(SourceSpan? sourceSpan, LiteralExpressionKind kind, object value)
    {
        SourceSpan = sourceSpan;
        Kind = kind;
        Value = value;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitLiteral(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitLiteral(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformLiteral(this);

    public IExpression Clone()
        => new LiteralExpression(SourceSpan, Kind, Value)
        {
            ReturnTypeMetadata = ReturnTypeMetadata,
        };

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public LiteralExpressionKind Kind { get; }

    public object Value { get; }

    public ITypeMetadata? ReturnTypeMetadata { get; set; }
}