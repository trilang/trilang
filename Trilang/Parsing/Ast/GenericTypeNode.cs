using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class GenericTypeNode : IInlineTypeNode
{
    public GenericTypeNode(string prefixName, IReadOnlyList<IInlineTypeNode> typeArguments)
    {
        PrefixName = prefixName;
        TypeArguments = typeArguments;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitGenericType(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformGenericType(this);

    public string PrefixName { get; }

    public IReadOnlyList<IInlineTypeNode> TypeArguments { get; }
}