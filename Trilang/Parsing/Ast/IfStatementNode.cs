using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class IfStatementNode : IStatementNode
{
    public IfStatementNode(IExpressionNode condition, BlockStatementNode then, BlockStatementNode? @else = null)
    {
        Condition = condition;
        Then = then;
        Else = @else;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitIf(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformIf(this);

    public IExpressionNode Condition { get; }

    public BlockStatementNode Then { get; }

    public BlockStatementNode? Else { get; }
}