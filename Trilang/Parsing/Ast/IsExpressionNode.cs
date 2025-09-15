using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

// TODO: replace by pattern matching
public class IsExpressionNode : IExpressionNode
{
    public IsExpressionNode(IExpressionNode expression, IInlineTypeNode type)
    {
        SourceSpan = expression.SourceSpan.Combine(type.SourceSpan);
        Expression = expression;
        Type = type;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitIsExpression(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformAsExpression(this);

    public SourceSpan SourceSpan { get; }

    public IExpressionNode Expression { get; }

    public IInlineTypeNode Type { get; }
}