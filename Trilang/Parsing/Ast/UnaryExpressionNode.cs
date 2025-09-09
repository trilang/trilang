using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class UnaryExpressionNode : IExpressionNode
{
    public UnaryExpressionNode(UnaryExpressionKind kind, IExpressionNode operand)
    {
        Kind = kind;
        Operand = operand;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitUnaryExpression(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformUnaryExpression(this);

    public UnaryExpressionKind Kind { get; }

    public IExpressionNode Operand { get; }
}