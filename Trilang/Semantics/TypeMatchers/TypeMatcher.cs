using Trilang.Metadata;

namespace Trilang.Semantics.TypeMatchers;

public class TypeMatcher
{
    private readonly BuiltInTypes builtInTypes;

    public TypeMatcher(BuiltInTypes builtInTypes)
        => this.builtInTypes = builtInTypes;

    public ITypeMatchResult Match(ITypeMetadata? actual, ITypeMetadata? expected)
    {
        actual = actual?.UnpackAlias();
        expected = expected?.UnpackAlias();

        if (actual is null || expected is null)
            return FailedMatch.Instance;

        if (actual.IsInvalid || expected.IsInvalid)
            return InvalidMatch.Instance;

        if (actual == expected)
            return SuccessMatch.Instance;

        if (expected is DiscriminatedUnionMetadata du)
        {
            foreach (var type in du.Types)
                if (Match(actual, type) is SuccessMatch successMatch)
                    return successMatch;

            return FailedMatch.Instance;
        }

        if (CanImplicitlyConvert(actual, expected))
            return new ImplicitConversion(expected);

        return FailedMatch.Instance;
    }

    public bool CanImplicitlyConvert(ITypeMetadata actual, ITypeMetadata expected)
    {
        if (actual == builtInTypes.F32 && expected == builtInTypes.F64)
            return true;

        if (GetSignedRank(actual) is { } actualSigned &&
            GetSignedRank(expected) is { } expectedSigned)
            return actualSigned < expectedSigned;

        if (GetUnsignedRank(actual) is { } actualUnsigned &&
            GetUnsignedRank(expected) is { } expectedUnsigned)
            return actualUnsigned < expectedUnsigned;

        return false;
    }

    private int? GetSignedRank(ITypeMetadata type)
    {
        if (type == builtInTypes.I8) return 0;
        if (type == builtInTypes.I16) return 1;
        if (type == builtInTypes.I32) return 2;
        if (type == builtInTypes.I64) return 3;

        return null;
    }

    private int? GetUnsignedRank(ITypeMetadata type)
    {
        if (type == builtInTypes.U8) return 0;
        if (type == builtInTypes.U16) return 1;
        if (type == builtInTypes.U32) return 2;
        if (type == builtInTypes.U64) return 3;

        return null;
    }

    public bool IsSigned(ITypeMetadata type)
        => type == builtInTypes.I8 ||
           type == builtInTypes.I16 ||
           type == builtInTypes.I32 ||
           type == builtInTypes.I64;

    public bool IsUnsigned(ITypeMetadata type)
        => type == builtInTypes.U8 ||
           type == builtInTypes.U16 ||
           type == builtInTypes.U32 ||
           type == builtInTypes.U64;

    public bool IsInteger(ITypeMetadata type)
        => IsSigned(type) || IsUnsigned(type);

    public bool IsFloat(ITypeMetadata type)
        => type == builtInTypes.F32 ||
           type == builtInTypes.F64;

    public bool IsNumber(ITypeMetadata type)
        => IsInteger(type) || IsFloat(type);

    public bool IsBool(ITypeMetadata type)
        => type == builtInTypes.Bool;
}