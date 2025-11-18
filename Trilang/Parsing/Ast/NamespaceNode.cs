using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

// TODO: make namespaces optional, derive it from the file path
public class NamespaceNode : ISyntaxNode
{
    public NamespaceNode(SourceSpan sourceSpan, IReadOnlyList<string> parts)
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
        => visitor.VisitNamespace(this);

    public TResult Transform<TResult>(INodeTransformer<TResult> transformer)
        => transformer.TransformNamespace(this);

    public SourceSpan SourceSpan { get; }

    public IReadOnlyList<string> Parts { get; }
}