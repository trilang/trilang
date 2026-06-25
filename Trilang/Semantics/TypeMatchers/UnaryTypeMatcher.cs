using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.TypeMatchers;

public class UnaryTypeMatcher
{
    private readonly TypeMatcher typeMatcher;

    public UnaryTypeMatcher(TypeMatcher typeMatcher)
        => this.typeMatcher = typeMatcher;

    public ITypeMatchResult Match(UnaryExpressionKind kind, ITypeMetadata? operand)
    {
        operand = operand?.UnpackAlias();

        if (operand is null)
            return FailedMatch.Instance;

        if (operand.IsInvalid)
            return InvalidMatch.Instance;

        if (kind is UnaryExpressionKind.UnaryMinus or UnaryExpressionKind.UnaryPlus && typeMatcher.IsNumber(operand))
            return SuccessMatch.Instance;

        if (kind is UnaryExpressionKind.LogicalNot && typeMatcher.IsBool(operand))
            return SuccessMatch.Instance;

        if (kind is UnaryExpressionKind.BitwiseNot && typeMatcher.IsInteger(operand))
            return SuccessMatch.Instance;

        if (kind is UnaryExpressionKind.AddressOf)
            return SuccessMatch.Instance;

        if (kind is UnaryExpressionKind.Dereference && operand is PointerMetadata)
            return SuccessMatch.Instance;

        return FailedMatch.Instance;
    }
}