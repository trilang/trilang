using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class FakeTypeNode : IInlineTypeNode
{
    public FakeTypeNode(SourceSpan sourceSpan, string name)
    {
        SourceSpan = sourceSpan;
        Name = name;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitFakeType(this);

    public TResult Transform<TResult>(INodeTransformer<TResult> transformer)
        => transformer.TransformFakeType(this);

    public SourceSpan SourceSpan { get; }

    public string Name { get; }
}
