using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class BinaryExpression : IExpression
{
    public BinaryExpression(BinaryExpressionKind kind, IExpression left, IExpression right)
    {
        Kind = kind;
        Left = left;
        Right = right;

        Left.Parent = this;
        Right.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitBinaryExpression(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitBinaryExpression(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformBinaryExpression(this);

    public IExpression Clone()
        => new BinaryExpression(Kind, Left.Clone(), Right.Clone())
        {
            ReturnTypeMetadata = ReturnTypeMetadata,
        };

    public ISemanticNode? Parent { get; set; }

    public BinaryExpressionKind Kind { get; }

    public IExpression Left { get; }

    public IExpression Right { get; }

    public ITypeMetadata? ReturnTypeMetadata { get; set; }

    public bool IsCompoundAssignment
        => Kind is
            BinaryExpressionKind.AdditionAssignment or
            BinaryExpressionKind.SubtractionAssignment or
            BinaryExpressionKind.MultiplicationAssignment or
            BinaryExpressionKind.DivisionAssignment or
            BinaryExpressionKind.ModulusAssignment or
            BinaryExpressionKind.BitwiseAndAssignment or
            BinaryExpressionKind.BitwiseOrAssignment or
            BinaryExpressionKind.BitwiseXorAssignment;
}