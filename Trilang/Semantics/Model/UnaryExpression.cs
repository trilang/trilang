using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class UnaryExpression : IExpression
{
    public UnaryExpression(SourceSpan? sourceSpan, UnaryExpressionKind kind, IExpression operand)
    {
        SourceSpan = sourceSpan;
        Kind = kind;
        Operand = operand;
        Operand.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitUnaryExpression(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitUnaryExpression(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformUnaryExpression(this);

    public IExpression Clone()
        => new UnaryExpression(SourceSpan, Kind, Operand.Clone())
        {
            ReturnTypeMetadata = ReturnTypeMetadata,
        };

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public UnaryExpressionKind Kind { get; }

    public IExpression Operand { get; }

    public ITypeMetadata? ReturnTypeMetadata { get; set; }
}