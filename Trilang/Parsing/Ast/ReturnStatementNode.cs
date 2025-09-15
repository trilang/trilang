using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class ReturnStatementNode : IStatementNode
{
    public ReturnStatementNode(SourceSpan sourceSpan, IExpressionNode? expression = null)
    {
        SourceSpan = sourceSpan;
        Expression = expression;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitReturn(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformReturn(this);

    public SourceSpan SourceSpan { get; }

    public IExpressionNode? Expression { get; }
}