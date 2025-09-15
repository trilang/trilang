using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class CallExpressionNode : IExpressionNode
{
    public CallExpressionNode(
        SourceSpan sourceSpan,
        IExpressionNode member,
        IReadOnlyList<IExpressionNode> parameters)
    {
        SourceSpan = sourceSpan;
        Member = member;
        Parameters = parameters;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitCall(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformCall(this);

    public SourceSpan SourceSpan { get; }

    public IExpressionNode Member { get; }

    public IReadOnlyList<IExpressionNode> Parameters { get; }
}