using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class FakeStatementNode : IStatementNode
{
    public FakeStatementNode(SourceSpan sourceSpan)
        => SourceSpan = sourceSpan;

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitFakeStatement(this);

    public TResult Transform<TResult>(INodeTransformer<TResult> transformer)
        => transformer.TransformFakeStatement(this);

    public SourceSpan SourceSpan { get; }
}