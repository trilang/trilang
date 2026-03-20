using System.Diagnostics;
using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class TypeRefNode : IInlineTypeNode
{
    public TypeRefNode(SourceSpan sourceSpan, IReadOnlyList<string> parts)
    {
        if (parts.Count == 0)
            throw new ArgumentException("Type must have at least one part", nameof(parts));

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
        => visitor.VisitTypeNode(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformTypeNode(this);

    public SourceSpan SourceSpan { get; }

    public IReadOnlyList<string> Parts { get; }
}