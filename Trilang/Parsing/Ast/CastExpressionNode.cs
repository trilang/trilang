using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class CastExpressionNode : IExpressionNode
{
    public CastExpressionNode(IInlineTypeNode type, IExpressionNode expression)
    {
        Type = type;
        Expression = expression;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitCast(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformCast(this);

    public IInlineTypeNode Type { get; }

    public IExpressionNode Expression { get; }
}