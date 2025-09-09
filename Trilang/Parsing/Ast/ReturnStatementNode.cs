using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class ReturnStatementNode : IStatementNode
{
    public ReturnStatementNode(IExpressionNode? expression = null)
        => Expression = expression;

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitReturn(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformReturn(this);

    public IExpressionNode? Expression { get; }
}