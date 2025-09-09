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