using Trilang.Metadata;
using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class BinaryExpressionNode : IExpressionNode, IEquatable<BinaryExpressionNode>
{
    public BinaryExpressionNode(
        BinaryExpressionKind kind,
        IExpressionNode left,
        IExpressionNode right)
    {
        Kind = kind;
        Left = left;
        Right = right;

        Left.Parent = this;
        Right.Parent = this;
    }

    public static bool operator ==(BinaryExpressionNode? left, BinaryExpressionNode? right)
        => Equals(left, right);

    public static bool operator !=(BinaryExpressionNode? left, BinaryExpressionNode? right)
        => !Equals(left, right);

    public bool Equals(BinaryExpressionNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Kind.Equals(other.Kind) &&
               Left.Equals(other.Left) &&
               Right.Equals(other.Right) &&
               Equals(ReturnTypeMetadata, other.ReturnTypeMetadata);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((BinaryExpressionNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Left, Right);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitBinaryExpression(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitBinaryExpression(this, context);

    public ISyntaxNode Transform(ITransformer transformer)
        => transformer.TransformBinaryExpression(this);

    public IExpressionNode Clone()
        => new BinaryExpressionNode(Kind, Left.Clone(), Right.Clone())
        {
            ReturnTypeMetadata = ReturnTypeMetadata,
        };

    public ISyntaxNode? Parent { get; set; }

    public BinaryExpressionKind Kind { get; }

    public IExpressionNode Left { get; }

    public IExpressionNode Right { get; }

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