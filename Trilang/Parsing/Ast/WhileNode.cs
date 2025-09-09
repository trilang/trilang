using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class WhileNode : IStatementNode
{
    public WhileNode(IExpressionNode condition, BlockStatementNode body)
    {
        Condition = condition;
        Body = body;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitWhile(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformWhile(this);

    public IExpressionNode Condition { get; }

    public BlockStatementNode Body { get; }
}