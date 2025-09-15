using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class ArrayAccessExpressionNode : IExpressionNode
{
    public ArrayAccessExpressionNode(SourceSpan sourceSpan, IExpressionNode member, IExpressionNode index)
    {
        SourceSpan = sourceSpan;
        Member = member;
        Index = index;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitArrayAccess(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformArrayAccess(this);

    public SourceSpan SourceSpan { get; }

    public IExpressionNode Member { get; }

    public IExpressionNode Index { get; }
}