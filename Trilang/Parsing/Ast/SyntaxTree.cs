using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class SyntaxTree : ISyntaxNode
{
    public SyntaxTree(
        SourceFile sourceFile,
        NamespaceNode namespaceNode,
        IReadOnlyList<UseNode> useNodes,
        IReadOnlyList<IDeclarationNode> declarations)
    {
        SourceFile = sourceFile;
        UseNodes = useNodes;
        Namespace = namespaceNode;
        Declarations = declarations;

        var lastNode = declarations.LastOrDefault() ??
                       namespaceNode as ISyntaxNode;

        SourceSpan = namespaceNode.SourceSpan.Combine(lastNode.SourceSpan);
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitTree(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformTree(this);

    public SourceFile SourceFile { get; }

    public SourceSpan SourceSpan { get; }

    public NamespaceNode Namespace { get; }

    public IReadOnlyList<UseNode> UseNodes { get; }

    public IReadOnlyList<IDeclarationNode> Declarations { get; }
}