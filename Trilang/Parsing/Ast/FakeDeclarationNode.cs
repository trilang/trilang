using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class FakeDeclarationNode : IDeclarationNode
{
    public FakeDeclarationNode(SourceSpan sourceSpan)
        => SourceSpan = sourceSpan;

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitFakeDeclaration(this);

    public TResult Transform<TResult>(INodeTransformer<TResult> transformer)
        => transformer.TransformFakeDeclaration(this);

    public SourceSpan SourceSpan { get; }
}
