using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class LiteralExpressionNode : IExpressionNode
{
    public LiteralExpressionNode(SourceSpan sourceSpan, LiteralExpressionKind kind, object value)
    {
        SourceSpan = sourceSpan;
        Kind = kind;
        Value = value;
    }

    public static LiteralExpressionNode Integer(SourceSpan sourceSpan, int number)
        => new LiteralExpressionNode(sourceSpan, LiteralExpressionKind.Integer, number);

    public static LiteralExpressionNode Float(SourceSpan sourceSpan, double number)
        => new LiteralExpressionNode(sourceSpan, LiteralExpressionKind.Float, number);

    public static LiteralExpressionNode True(SourceSpan sourceSpan)
        => new LiteralExpressionNode(sourceSpan, LiteralExpressionKind.Boolean, true);

    public static LiteralExpressionNode False(SourceSpan sourceSpan)
        => new LiteralExpressionNode(sourceSpan, LiteralExpressionKind.Boolean, false);

    public static LiteralExpressionNode String(SourceSpan sourceSpan, string str)
        => new LiteralExpressionNode(sourceSpan, LiteralExpressionKind.String, str);

    public static LiteralExpressionNode Char(SourceSpan sourceSpan, string c)
        => new LiteralExpressionNode(sourceSpan, LiteralExpressionKind.Char, c);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitLiteral(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformLiteral(this);

    public SourceSpan SourceSpan { get; }

    public LiteralExpressionKind Kind { get; }

    public object Value { get; }
}