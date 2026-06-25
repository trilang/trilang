namespace Trilang.Semantics.Model;

public enum BinaryExpressionKind
{
    Unknown,

    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulus,

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
    GreaterThanOrEqual,

    Assignment,

    AdditionAssignment,
    SubtractionAssignment,
    MultiplicationAssignment,
    DivisionAssignment,
    ModulusAssignment,

    BitwiseAndAssignment,
    BitwiseOrAssignment,
    BitwiseXorAssignment,
}

public static class BinaryExpressionKindExtensions
{
    public static bool IsArithmetic(this BinaryExpressionKind kind)
        => kind is BinaryExpressionKind.Addition
            or BinaryExpressionKind.Subtraction
            or BinaryExpressionKind.Multiplication
            or BinaryExpressionKind.Division
            or BinaryExpressionKind.Modulus;

    public static bool IsBitwise(this BinaryExpressionKind kind)
        => kind is BinaryExpressionKind.BitwiseAnd
            or BinaryExpressionKind.BitwiseOr
            or BinaryExpressionKind.BitwiseXor;

    public static bool IsConditional(this BinaryExpressionKind kind)
        => kind is BinaryExpressionKind.ConditionalAnd
            or BinaryExpressionKind.ConditionalOr;

    public static bool IsEquality(this BinaryExpressionKind kind)
        => kind is BinaryExpressionKind.Equality
            or BinaryExpressionKind.Inequality;

    public static bool IsRelational(this BinaryExpressionKind kind)
        => kind is BinaryExpressionKind.LessThan
            or BinaryExpressionKind.LessThanOrEqual
            or BinaryExpressionKind.GreaterThan
            or BinaryExpressionKind.GreaterThanOrEqual;

    public static bool IsAssignment(this BinaryExpressionKind kind)
        => kind is BinaryExpressionKind.Assignment;

    public static bool IsArithmeticCompoundAssignment(this BinaryExpressionKind kind)
        => kind is BinaryExpressionKind.AdditionAssignment
            or BinaryExpressionKind.SubtractionAssignment
            or BinaryExpressionKind.MultiplicationAssignment
            or BinaryExpressionKind.DivisionAssignment
            or BinaryExpressionKind.ModulusAssignment;

    public static bool IsBitwiseCompoundAssignment(this BinaryExpressionKind kind)
        => kind is BinaryExpressionKind.BitwiseAndAssignment
            or BinaryExpressionKind.BitwiseOrAssignment
            or BinaryExpressionKind.BitwiseXorAssignment;

    public static bool IsCompoundAssignment(this BinaryExpressionKind kind)
        => IsArithmeticCompoundAssignment(kind) ||
           IsBitwiseCompoundAssignment(kind);
}