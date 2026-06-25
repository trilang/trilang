using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Semantics.TypeMatchers;
using static Trilang.Semantics.Model.BinaryExpressionKind;

namespace Tri.Tests.Semantics;

public class BinaryTypeMatcherTests
{
    [Test]
    public void MatchWithNullLeftFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new BinaryTypeMatcher(typeMatcher);

        var result = matcher.Match(Addition, null, builtInTypes.I32);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void MatchWithNullRightFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new BinaryTypeMatcher(typeMatcher);

        var result = matcher.Match(Addition, builtInTypes.I32, null);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void MatchWithBothNullFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new BinaryTypeMatcher(typeMatcher);

        var result = matcher.Match(Addition, null, null);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void MatchWithInvalidLeftFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new BinaryTypeMatcher(typeMatcher);
        var invalidType = new TypeMetadata(null, "invalid", [], [], [], [], [], [], true, false);
        invalidType.MarkAsInvalid();

        var result = matcher.Match(Addition, invalidType, builtInTypes.I32);

        Assert.That(result, Is.EqualTo(InvalidMatch.Instance));
    }

    [Test]
    public void MatchWithRightNullFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new BinaryTypeMatcher(typeMatcher);
        var invalidType = new TypeMetadata(null, "invalid", [], [], [], [], [], [], true, false);
        invalidType.MarkAsInvalid();

        var result = matcher.Match(Addition, builtInTypes.I32, invalidType);

        Assert.That(result, Is.EqualTo(InvalidMatch.Instance));
    }

    [Test]
    public void MatchWithRightBothFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new BinaryTypeMatcher(typeMatcher);
        var invalidType = new TypeMetadata(null, "invalid", [], [], [], [], [], [], true, false);
        invalidType.MarkAsInvalid();

        var result = matcher.Match(Addition, invalidType, invalidType);

        Assert.That(result, Is.EqualTo(InvalidMatch.Instance));
    }

    private static IEnumerable<object[]> MatchWithTheSameTypesReturnsSuccessData()
    {
        var builtInTypes = new BuiltInTypes();

        // arithmetic
        yield return [builtInTypes, Addition, builtInTypes.I32, builtInTypes.I32];
        yield return [builtInTypes, Subtraction, builtInTypes.I32, builtInTypes.I32];
        yield return [builtInTypes, Multiplication, builtInTypes.I32, builtInTypes.I32];
        yield return [builtInTypes, Division, builtInTypes.I32, builtInTypes.I32];
        yield return [builtInTypes, Modulus, builtInTypes.I32, builtInTypes.I32];

        // bitwise
        yield return [builtInTypes, BitwiseAnd, builtInTypes.I32, builtInTypes.I32];
        yield return [builtInTypes, BitwiseOr, builtInTypes.I32, builtInTypes.I32];
        yield return [builtInTypes, BitwiseXor, builtInTypes.I32, builtInTypes.I32];

        //conditional
        yield return [builtInTypes, ConditionalAnd, builtInTypes.Bool, builtInTypes.Bool];
        yield return [builtInTypes, ConditionalOr, builtInTypes.Bool, builtInTypes.Bool];

        // equality
        yield return [builtInTypes, Equality, builtInTypes.I32, builtInTypes.I32];
        yield return [builtInTypes, Inequality, builtInTypes.I32, builtInTypes.I32];

        // rational
        yield return [builtInTypes, LessThan, builtInTypes.I32, builtInTypes.I32];
        yield return [builtInTypes, LessThanOrEqual, builtInTypes.I32, builtInTypes.I32];
        yield return [builtInTypes, GreaterThan, builtInTypes.I32, builtInTypes.I32];
        yield return [builtInTypes, GreaterThanOrEqual, builtInTypes.I32, builtInTypes.I32];

        // assignment
        yield return [builtInTypes, Assignment, builtInTypes.I32, builtInTypes.I32];

        // arithmetic compound assignment
        yield return [builtInTypes, AdditionAssignment, builtInTypes.I32, builtInTypes.I32];
        yield return [builtInTypes, SubtractionAssignment, builtInTypes.I32, builtInTypes.I32];
        yield return [builtInTypes, MultiplicationAssignment, builtInTypes.I32, builtInTypes.I32];
        yield return [builtInTypes, DivisionAssignment, builtInTypes.I32, builtInTypes.I32];
        yield return [builtInTypes, ModulusAssignment, builtInTypes.I32, builtInTypes.I32];

        // bitwise compound assignment
        yield return [builtInTypes, BitwiseAndAssignment, builtInTypes.I32, builtInTypes.I32];
        yield return [builtInTypes, BitwiseOrAssignment, builtInTypes.I32, builtInTypes.I32];
        yield return [builtInTypes, BitwiseXorAssignment, builtInTypes.I32, builtInTypes.I32];
    }

    [Test]
    [TestCaseSource(nameof(MatchWithTheSameTypesReturnsSuccessData))]
    public void MatchWithTheSameTypesReturnsSuccess(
        BuiltInTypes builtInTypes,
        BinaryExpressionKind kind,
        ITypeMetadata left,
        ITypeMetadata right)
    {
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new BinaryTypeMatcher(typeMatcher);

        var result = matcher.Match(kind, left, right);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    private static IEnumerable<object[]> MatchWithTheDifferentTypesReturnsFailedData()
    {
        var builtInTypes = new BuiltInTypes();

        // arithmetic
        yield return [builtInTypes, Addition, builtInTypes.I32, builtInTypes.F32];
        yield return [builtInTypes, Addition, builtInTypes.F32, builtInTypes.I32];

        // bitwise
        yield return [builtInTypes, BitwiseAnd, builtInTypes.I32, builtInTypes.U32];
        yield return [builtInTypes, BitwiseAnd, builtInTypes.U32, builtInTypes.I32];

        //conditional
        yield return [builtInTypes, ConditionalAnd, builtInTypes.Bool, builtInTypes.F32];
        yield return [builtInTypes, ConditionalAnd, builtInTypes.F32, builtInTypes.Bool];

        // rational
        yield return [builtInTypes, LessThan, builtInTypes.I32, builtInTypes.Bool];
        yield return [builtInTypes, LessThan, builtInTypes.Bool, builtInTypes.I32];

        // arithmetic compound assignment
        yield return [builtInTypes, AdditionAssignment, builtInTypes.I32, builtInTypes.Bool];
        yield return [builtInTypes, AdditionAssignment, builtInTypes.Bool, builtInTypes.I32];

        // bitwise compound assignment
        yield return [builtInTypes, BitwiseAndAssignment, builtInTypes.I32, builtInTypes.F32];
        yield return [builtInTypes, BitwiseAndAssignment, builtInTypes.F32, builtInTypes.I32];
    }

    [Test]
    [TestCaseSource(nameof(MatchWithTheDifferentTypesReturnsFailedData))]
    public void MatchWithTheDifferentTypesReturnsFailed(
        BuiltInTypes builtInTypes,
        BinaryExpressionKind kind,
        ITypeMetadata left,
        ITypeMetadata right)
    {
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new BinaryTypeMatcher(typeMatcher);

        var result = matcher.Match(kind, left, right);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    private static IEnumerable<TestCaseData> MatchWithCastReturnsImplicitConversionData()
    {
        var builtInTypes = new BuiltInTypes();

        var integerKinds = new[]
        {
            Addition,
            Subtraction,
            Multiplication,
            Division,
            Modulus,
            BitwiseAnd,
            BitwiseOr,
            BitwiseXor,
            LessThan,
            LessThanOrEqual,
            GreaterThan,
            GreaterThanOrEqual,
            Equality,
            Inequality,
            Assignment,
            AdditionAssignment,
            SubtractionAssignment,
            MultiplicationAssignment,
            DivisionAssignment,
            ModulusAssignment,
            BitwiseAndAssignment,
            BitwiseOrAssignment,
            BitwiseXorAssignment,
        };

        var floatKinds = new[]
        {
            Addition,
            Subtraction,
            Multiplication,
            Division,
            Modulus,
            LessThan,
            LessThanOrEqual,
            GreaterThan,
            GreaterThanOrEqual,
            Equality,
            Inequality,
            Assignment,
            AdditionAssignment,
            SubtractionAssignment,
            MultiplicationAssignment,
            DivisionAssignment,
            ModulusAssignment,
        };

        var signedPairs = new (ITypeMetadata, ITypeMetadata)[]
        {
            (builtInTypes.I8, builtInTypes.I16),
            (builtInTypes.I8, builtInTypes.I32),
            (builtInTypes.I8, builtInTypes.I64),
            (builtInTypes.I16, builtInTypes.I32),
            (builtInTypes.I16, builtInTypes.I64),
            (builtInTypes.I32, builtInTypes.I64),
        };

        var unsignedPairs = new (ITypeMetadata, ITypeMetadata)[]
        {
            (builtInTypes.U8, builtInTypes.U16),
            (builtInTypes.U8, builtInTypes.U32),
            (builtInTypes.U8, builtInTypes.U64),
            (builtInTypes.U16, builtInTypes.U32),
            (builtInTypes.U16, builtInTypes.U64),
            (builtInTypes.U32, builtInTypes.U64),
        };

        foreach (var kind in integerKinds)
        foreach (var (smaller, larger) in signedPairs)
        {
            yield return new TestCaseData(builtInTypes, kind, smaller, larger)
                .Returns(new BinaryImplicitConversion(larger, null));
            yield return new TestCaseData(builtInTypes, kind, larger, smaller)
                .Returns(new BinaryImplicitConversion(null, larger));
        }

        foreach (var kind in integerKinds)
        foreach (var (smaller, larger) in unsignedPairs)
        {
            yield return new TestCaseData(builtInTypes, kind, smaller, larger)
                .Returns(new BinaryImplicitConversion(larger, null));
            yield return new TestCaseData(builtInTypes, kind, larger, smaller)
                .Returns(new BinaryImplicitConversion(null, larger));
        }

        foreach (var kind in floatKinds)
        {
            yield return new TestCaseData(builtInTypes, kind, builtInTypes.F32, builtInTypes.F64)
                .Returns(new BinaryImplicitConversion(builtInTypes.F64, null));
            yield return new TestCaseData(builtInTypes, kind, builtInTypes.F64, builtInTypes.F32)
                .Returns(new BinaryImplicitConversion(null, builtInTypes.F64));
        }
    }

    [Test]
    [TestCaseSource(nameof(MatchWithCastReturnsImplicitConversionData))]
    public ITypeMatchResult MatchWithCastReturnsImplicitConversion(
        BuiltInTypes builtInTypes,
        BinaryExpressionKind kind,
        ITypeMetadata left,
        ITypeMetadata right)
    {
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new BinaryTypeMatcher(typeMatcher);

        return matcher.Match(kind, left, right);
    }
}