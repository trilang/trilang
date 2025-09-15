using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class WhileNode : IStatementNode
{
    public WhileNode(SourceSpan sourceSpan, IExpressionNode condition, BlockStatementNode body)
    {
        SourceSpan = sourceSpan;
        Condition = condition;
        Body = body;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitWhile(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformWhile(this);

    public SourceSpan SourceSpan { get; }

    public IExpressionNode Condition { get; }

    public BlockStatementNode Body { get; }
}