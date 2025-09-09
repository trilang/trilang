using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class LiteralExpression : IExpression
{
    public LiteralExpression(LiteralExpressionKind kind, object value)
    {
        Kind = kind;
        Value = value;
    }

    public static LiteralExpression Integer(int number)
        => new LiteralExpression(LiteralExpressionKind.Integer, number);

    public static LiteralExpression Float(double number)
        => new LiteralExpression(LiteralExpressionKind.Float, number);

    public static LiteralExpression True()
        => new LiteralExpression(LiteralExpressionKind.Boolean, true);

    public static LiteralExpression False()
        => new LiteralExpression(LiteralExpressionKind.Boolean, false);

    public static LiteralExpression String(string str)
        => new LiteralExpression(LiteralExpressionKind.String, str);

    public static LiteralExpression Char(char c)
        => new LiteralExpression(LiteralExpressionKind.Char, c);

    public void Accept(IVisitor visitor)
        => visitor.VisitLiteral(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitLiteral(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformLiteral(this);

    public IExpression Clone()
        => new LiteralExpression(Kind, Value)
        {
            ReturnTypeMetadata = ReturnTypeMetadata,
        };

    public ISemanticNode? Parent { get; set; }

    public LiteralExpressionKind Kind { get; }

    public object Value { get; }

    public ITypeMetadata? ReturnTypeMetadata { get; set; }
}