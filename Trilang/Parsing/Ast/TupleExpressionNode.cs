using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class TupleExpressionNode : IExpressionNode
{
    public TupleExpressionNode(IReadOnlyList<IExpressionNode> expressions)
    {
        if (expressions.Count <= 1)
            throw new ArgumentException("Tuple must have at least 2 elements", nameof(expressions));

        Expressions = expressions;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitTuple(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformTuple(this);

    public IReadOnlyList<IExpressionNode> Expressions { get; }
}