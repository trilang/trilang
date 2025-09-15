using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class ExpressionStatementNode : IStatementNode
{
    public ExpressionStatementNode(SourceSpan sourceSpan, IExpressionNode expression)
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
        => visitor.VisitExpressionStatement(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformExpressionStatement(this);

    public SourceSpan SourceSpan { get; }

    public IExpressionNode Expression { get; }
}