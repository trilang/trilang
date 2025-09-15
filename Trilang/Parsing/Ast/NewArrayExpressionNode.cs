using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class NewArrayExpressionNode : IExpressionNode
{
    public NewArrayExpressionNode(
        SourceSpan sourceSpan,
        ArrayTypeNode type,
        IExpressionNode size)
    {
        SourceSpan = sourceSpan;
        Type = type;
        Size = size;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitNewArray(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformNewArray(this);

    public SourceSpan SourceSpan { get; }

    public ArrayTypeNode Type { get; }

    public IExpressionNode Size { get; }
}