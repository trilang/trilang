using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class IfDirectiveNode : IDeclarationNode, IStatementNode
{
    public IfDirectiveNode(
        SourceSpan sourceSpan,
        string directiveName,
        IReadOnlyList<ISyntaxNode> then,
        IReadOnlyList<ISyntaxNode> @else)
    {
        SourceSpan = sourceSpan;
        DirectiveName = directiveName;
        Then = then;
        Else = @else;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitIfDirective(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformIfDirective(this);

    public SourceSpan SourceSpan { get; }

    public string DirectiveName { get; }

    // IDeclarationNode | IStatementNode
    public IReadOnlyList<ISyntaxNode> Then { get; }

    public IReadOnlyList<ISyntaxNode> Else { get; }
}