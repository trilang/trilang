using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class LiteralExpressionNode : IExpressionNode
{
    public LiteralExpressionNode(LiteralExpressionKind kind, object value)
    {
        Kind = kind;
        Value = value;
    }

    public static LiteralExpressionNode Integer(int number)
        => new LiteralExpressionNode(LiteralExpressionKind.Integer, number);

    public static LiteralExpressionNode Float(double number)
        => new LiteralExpressionNode(LiteralExpressionKind.Float, number);

    public static LiteralExpressionNode True()
        => new LiteralExpressionNode(LiteralExpressionKind.Boolean, true);

    public static LiteralExpressionNode False()
        => new LiteralExpressionNode(LiteralExpressionKind.Boolean, false);

    public static LiteralExpressionNode String(string str)
        => new LiteralExpressionNode(LiteralExpressionKind.String, str);

    public static LiteralExpressionNode Char(char c)
        => new LiteralExpressionNode(LiteralExpressionKind.Char, c);

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

    public LiteralExpressionKind Kind { get; }

    public object Value { get; }
}