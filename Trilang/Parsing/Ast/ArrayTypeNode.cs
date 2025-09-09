using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class ArrayTypeNode : IInlineTypeNode
{
    public ArrayTypeNode(IInlineTypeNode elementType)
        => ElementType = elementType;

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitArrayType(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformArrayType(this);

    public IInlineTypeNode ElementType { get; }
}