using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class PointerTypeNode : IInlineTypeNode
{
    public PointerTypeNode(SourceSpan sourceSpan, IInlineTypeNode type)
    {
        SourceSpan = sourceSpan;
        Type = type;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitPointer(this);

    public TResult Transform<TResult>(INodeTransformer<TResult> transformer)
        => transformer.TransformPointer(this);

    public SourceSpan SourceSpan { get; }

    public IInlineTypeNode Type { get; }
}