using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class NewObjectExpressionNode : IExpressionNode
{
    public NewObjectExpressionNode(SourceSpan sourceSpan, IExpressionNode member)
    {
        SourceSpan = sourceSpan;
        Member = member;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitNewObject(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformNewObject(this);

    public SourceSpan SourceSpan { get; }

    public IExpressionNode Member { get; }
}