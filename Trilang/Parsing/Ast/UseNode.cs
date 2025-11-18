using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class UseNode : ISyntaxNode
{
    public UseNode(SourceSpan sourceSpan, IReadOnlyList<string> parts)
    {
        SourceSpan = sourceSpan;
        Parts = parts;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitUse(this);

    public TResult Transform<TResult>(INodeTransformer<TResult> transformer)
        => transformer.TransformUse(this);

    public SourceSpan SourceSpan { get; }

    public IReadOnlyList<string> Parts { get; }
}