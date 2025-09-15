using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class DiscriminatedUnionNode : IInlineTypeNode
{
    public DiscriminatedUnionNode(IReadOnlyList<IInlineTypeNode> types)
    {
        if (types.Count < 2)
            throw new ArgumentException("Discriminated union must have at least 2 elements", nameof(types));

        SourceSpan = types[0].SourceSpan.Combine(types[^1].SourceSpan);
        Types = types;
    }

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

    public SourceSpan SourceSpan { get; }

    public IReadOnlyList<IInlineTypeNode> Types { get; }
}