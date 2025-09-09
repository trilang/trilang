using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class DiscriminatedUnionNode : IInlineTypeNode
{
    public DiscriminatedUnionNode(IReadOnlyList<IInlineTypeNode> types)
        => Types = types;

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitDiscriminatedUnion(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformDiscriminatedUnion(this);

    public IReadOnlyList<IInlineTypeNode> Types { get; }
}