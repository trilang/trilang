using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class BinaryExpressionNode : IExpressionNode
{
    public BinaryExpressionNode(BinaryExpressionKind kind, IExpressionNode left, IExpressionNode right)
    {
        Kind = kind;
        Left = left;
        Right = right;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitBinaryExpression(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformBinaryExpression(this);

    public BinaryExpressionKind Kind { get; }

    public IExpressionNode Left { get; }

    public IExpressionNode Right { get; }
}