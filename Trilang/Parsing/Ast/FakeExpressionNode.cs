using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class FakeExpressionNode : IExpressionNode
{
    public FakeExpressionNode(SourceSpan span)
        => SourceSpan = span;

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitFakeExpression(this);

    public TResult Transform<TResult>(INodeTransformer<TResult> transformer)
        => transformer.TransformFakeExpression(this);

    public SourceSpan SourceSpan { get; }
}