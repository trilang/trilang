using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class TypeRefNode : IInlineTypeNode
{
    public TypeRefNode(SourceSpan sourceSpan, string name)
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
        => visitor.VisitTypeNode(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformTypeNode(this);

    public SourceSpan SourceSpan { get; }

    public string Name { get; }
}