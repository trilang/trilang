namespace Trilang.Parsing.Nodes;

public enum BinaryExpressionKind
{
    Unknown,

    Addition,
    Subtraction,
    Multiplication,
    Division,

    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,

    ConditionalAnd,
    ConditionalOr,
    Equality,
    Inequality,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual
}

public static class BinaryOperatorKindExtensions
{
    public static int GetPrecedence(this BinaryExpressionKind kind)
        => kind switch
        {
            BinaryExpressionKind.ConditionalOr => 70,
            BinaryExpressionKind.ConditionalAnd => 71,

            BinaryExpressionKind.BitwiseOr => 80,
            BinaryExpressionKind.BitwiseXor => 81,
            BinaryExpressionKind.BitwiseAnd => 82,

            BinaryExpressionKind.Equality or BinaryExpressionKind.Inequality => 90,
            BinaryExpressionKind.LessThan or
                BinaryExpressionKind.LessThanOrEqual or
                BinaryExpressionKind.GreaterThan or
                BinaryExpressionKind.GreaterThanOrEqual => 91,

            BinaryExpressionKind.Addition or BinaryExpressionKind.Subtraction => 100,
            BinaryExpressionKind.Multiplication or BinaryExpressionKind.Division => 101,

            _ => throw new ArgumentOutOfRangeException(nameof(kind), kind, "Unknown binary operator."),
        };
}