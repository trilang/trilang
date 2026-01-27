using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class GenericApplicationNode : IInlineTypeNode
{
    public GenericApplicationNode(
        SourceSpan sourceSpan,
        string prefixName,
        IReadOnlyList<IInlineTypeNode> typeArguments)
    {
        SourceSpan = sourceSpan;
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

    public SourceSpan SourceSpan { get; }

    public string PrefixName { get; }

    public IReadOnlyList<IInlineTypeNode> TypeArguments { get; }
}