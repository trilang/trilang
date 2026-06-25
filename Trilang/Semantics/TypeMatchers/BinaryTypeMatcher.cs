using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.TypeMatchers;

public class BinaryTypeMatcher
{
    private readonly TypeMatcher typeMatcher;

    public BinaryTypeMatcher(TypeMatcher typeMatcher)
        => this.typeMatcher = typeMatcher;

    public ITypeMatchResult Match(BinaryExpressionKind kind, ITypeMetadata? left, ITypeMetadata? right)
    {
        left = left?.UnpackAlias();
        right = right?.UnpackAlias();

        if (left is null || right is null)
            return FailedMatch.Instance;

        if (left.IsInvalid || right.IsInvalid)
            return InvalidMatch.Instance;

        var canCastLeft = typeMatcher.CanImplicitlyConvert(left, right);
        var canCastRight = typeMatcher.CanImplicitlyConvert(right, left);

        if (!canCastLeft && !canCastRight && left != right)
            return FailedMatch.Instance;

        var result = canCastLeft
            ? new BinaryImplicitConversion(right, null)
            : canCastRight
                ? new BinaryImplicitConversion(null, left)
                : (ITypeMatchResult)SuccessMatch.Instance;

        if (kind.IsArithmetic() && typeMatcher.IsNumber(left) && typeMatcher.IsNumber(right))
            return result;

        if (kind.IsBitwise() && typeMatcher.IsInteger(left) && typeMatcher.IsInteger(right))
            return result;

        if (kind.IsConditional() && typeMatcher.IsBool(left) && typeMatcher.IsBool(right))
            return result;

        if (kind.IsEquality())
            return result;

        if (kind.IsRelational() && typeMatcher.IsNumber(left) && typeMatcher.IsNumber(right))
            return result;

        if (kind.IsAssignment())
            return result;

        if (kind.IsArithmeticCompoundAssignment() && typeMatcher.IsNumber(left) && typeMatcher.IsNumber(right))
            return result;

        if (kind.IsBitwiseCompoundAssignment() && typeMatcher.IsInteger(left) && typeMatcher.IsInteger(right))
            return result;

        return FailedMatch.Instance;
    }
}