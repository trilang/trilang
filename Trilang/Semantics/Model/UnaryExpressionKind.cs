namespace Trilang.Semantics.Model;

public enum UnaryExpressionKind
{
    Unknown,

    UnaryMinus,
    UnaryPlus,

    LogicalNot,
    BitwiseNot,

    AddressOf,
    Dereference,
}

public static class UnaryExpressionKindExtensions
{
    public static string ToDisplayString(this UnaryExpressionKind kind)
        => kind switch
        {
            UnaryExpressionKind.UnaryMinus => "-",
            UnaryExpressionKind.UnaryPlus => "+",
            UnaryExpressionKind.LogicalNot => "!",
            UnaryExpressionKind.BitwiseNot => "~",
            UnaryExpressionKind.AddressOf => "&",
            UnaryExpressionKind.Dereference => "*",
            _ => "Unknown",
        };
}