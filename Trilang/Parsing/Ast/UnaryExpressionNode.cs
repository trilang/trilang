using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class UnaryExpressionNode : IExpressionNode
{
    public UnaryExpressionNode(SourceSpan sourceSpan, UnaryExpressionKind kind, IExpressionNode operand)
    {
        SourceSpan = sourceSpan;
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

    public SourceSpan SourceSpan { get; }

    public UnaryExpressionKind Kind { get; }

    public IExpressionNode Operand { get; }
}