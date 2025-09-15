using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class TupleTypeNode : IInlineTypeNode
{
    public TupleTypeNode(SourceSpan sourceSpan, IReadOnlyList<IInlineTypeNode> types)
    {
        if (types.Count <= 1)
            throw new ArgumentException("Tuple must have at least 2 elements", nameof(types));

        SourceSpan = sourceSpan;
        Types = types;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitTupleType(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformTupleType(this);

    public SourceSpan SourceSpan { get; }

    public IReadOnlyList<IInlineTypeNode> Types { get; }
}