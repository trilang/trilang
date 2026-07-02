using Trilang.Metadata;
using Trilang.Semantics.TypeMatchers;

namespace Tri.Tests.Semantics;

public class TypeMatcherTests
{
    private static IEnumerable<object[]> GetMatchingCasesWithTheSameType()
    {
        var builtInTypes = new BuiltInTypes();

        yield return [builtInTypes, builtInTypes.I8, builtInTypes.I8];
        yield return [builtInTypes, builtInTypes.I16, builtInTypes.I16];
        yield return [builtInTypes, builtInTypes.I32, builtInTypes.I32];
        yield return [builtInTypes, builtInTypes.I64, builtInTypes.I64];
        yield return [builtInTypes, builtInTypes.U8, builtInTypes.U8];
        yield return [builtInTypes, builtInTypes.U16, builtInTypes.U16];
        yield return [builtInTypes, builtInTypes.U32, builtInTypes.U32];
        yield return [builtInTypes, builtInTypes.U64, builtInTypes.U64];
        yield return [builtInTypes, builtInTypes.F32, builtInTypes.F32];
        yield return [builtInTypes, builtInTypes.F64, builtInTypes.F64];
        yield return [builtInTypes, builtInTypes.Bool, builtInTypes.Bool];
        yield return [builtInTypes, builtInTypes.Char, builtInTypes.Char];
        yield return [builtInTypes, builtInTypes.String, builtInTypes.String];
        yield return [builtInTypes, builtInTypes.Void, builtInTypes.Void];
        yield return [builtInTypes, builtInTypes.Null, builtInTypes.Null];
    }

    [Test]
    [TestCaseSource(nameof(GetMatchingCasesWithTheSameType))]
    public void SameTypeReturnsSuccessfulMatch(
        BuiltInTypes builtInTypes,
        ITypeMetadata actual,
        ITypeMetadata expected)
    {
        var typeMatcher = new TypeMatcher(new BuiltInTypes());

        var result = typeMatcher.Match(actual, expected);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    private static IEnumerable<object[]> GetFailedCasesWithDifferentTypes()
    {
        var builtInTypes = new BuiltInTypes();

        yield return [builtInTypes, builtInTypes.I64, builtInTypes.I32];
        yield return [builtInTypes, builtInTypes.I32, builtInTypes.I16];
        yield return [builtInTypes, builtInTypes.I16, builtInTypes.I8];
        yield return [builtInTypes, builtInTypes.U64, builtInTypes.U32];
        yield return [builtInTypes, builtInTypes.U32, builtInTypes.U16];
        yield return [builtInTypes, builtInTypes.U16, builtInTypes.U8];
        yield return [builtInTypes, builtInTypes.F64, builtInTypes.F32];
        yield return [builtInTypes, builtInTypes.I8, builtInTypes.U8];
        yield return [builtInTypes, builtInTypes.U8, builtInTypes.I8];
        yield return [builtInTypes, builtInTypes.I16, builtInTypes.U16];
        yield return [builtInTypes, builtInTypes.U16, builtInTypes.I16];
        yield return [builtInTypes, builtInTypes.I32, builtInTypes.U32];
        yield return [builtInTypes, builtInTypes.U32, builtInTypes.I32];
        yield return [builtInTypes, builtInTypes.I64, builtInTypes.U64];
        yield return [builtInTypes, builtInTypes.U64, builtInTypes.I64];
        yield return [builtInTypes, builtInTypes.I32, builtInTypes.F32];
        yield return [builtInTypes, builtInTypes.F32, builtInTypes.I32];
        yield return [builtInTypes, builtInTypes.I32, builtInTypes.F64];
        yield return [builtInTypes, builtInTypes.F64, builtInTypes.I32];
        yield return [builtInTypes, builtInTypes.I32, builtInTypes.Bool];
        yield return [builtInTypes, builtInTypes.Char, builtInTypes.Bool];
        yield return [builtInTypes, builtInTypes.Void, builtInTypes.I32];
        yield return [builtInTypes, builtInTypes.Null, builtInTypes.I32];
        yield return [builtInTypes, builtInTypes.I32, builtInTypes.Null];
    }

    [Test]
    [TestCaseSource(nameof(GetFailedCasesWithDifferentTypes))]
    public void DifferentTypesReturnsFailedMatch(
        BuiltInTypes builtInTypes,
        ITypeMetadata actual,
        ITypeMetadata expected)
    {
        var typeMatcher = new TypeMatcher(builtInTypes);

        var result = typeMatcher.Match(actual, expected);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    private static IEnumerable<object[]> GetImplicitConversionCases()
    {
        var builtInTypes = new BuiltInTypes();

        yield return [builtInTypes, builtInTypes.I8, builtInTypes.I16];
        yield return [builtInTypes, builtInTypes.I8, builtInTypes.I32];
        yield return [builtInTypes, builtInTypes.I8, builtInTypes.I64];
        yield return [builtInTypes, builtInTypes.I16, builtInTypes.I32];
        yield return [builtInTypes, builtInTypes.I16, builtInTypes.I64];
        yield return [builtInTypes, builtInTypes.I32, builtInTypes.I64];
        yield return [builtInTypes, builtInTypes.U8, builtInTypes.U16];
        yield return [builtInTypes, builtInTypes.U8, builtInTypes.U32];
        yield return [builtInTypes, builtInTypes.U8, builtInTypes.U64];
        yield return [builtInTypes, builtInTypes.U16, builtInTypes.U32];
        yield return [builtInTypes, builtInTypes.U16, builtInTypes.U64];
        yield return [builtInTypes, builtInTypes.U32, builtInTypes.U64];
        yield return [builtInTypes, builtInTypes.F32, builtInTypes.F64];
    }

    [Test]
    [TestCaseSource(nameof(GetImplicitConversionCases))]
    public void ImplicitConversionReturnsConversionMatch(
        BuiltInTypes builtInTypes,
        ITypeMetadata actual,
        ITypeMetadata expected)
    {
        var typeMatcher = new TypeMatcher(builtInTypes);

        var result = typeMatcher.Match(actual, expected);

        var implicitConversion = new ImplicitConversion(expected);

        Assert.That(result, Is.EqualTo(implicitConversion));
    }

    [Test]
    public void InvalidActualReturnsFailedMatch()
    {
        var builtInTypes = new BuiltInTypes();
        var invalidType = new TypeMetadata(null, AccessModifierMetadata.Public, "invalid", [], [], [], [], [], [], true, false);
        invalidType.MarkAsInvalid();
        var typeMatcher = new TypeMatcher(builtInTypes);

        var result = typeMatcher.Match(invalidType, builtInTypes.I32);

        Assert.That(result, Is.EqualTo(InvalidMatch.Instance));
    }

    [Test]
    public void InvalidExpectedReturnsFailedMatch()
    {
        var builtInTypes = new BuiltInTypes();
        var invalidType = new TypeMetadata(null, AccessModifierMetadata.Public, "invalid", [], [], [], [], [], [], true, false);
        invalidType.MarkAsInvalid();
        var typeMatcher = new TypeMatcher(builtInTypes);

        var result = typeMatcher.Match(builtInTypes.I32, invalidType);

        Assert.That(result, Is.EqualTo(InvalidMatch.Instance));
    }

    [Test]
    public void BothInvalidReturnsFailedMatch()
    {
        var builtInTypes = new BuiltInTypes();
        var invalidType = new TypeMetadata(null, AccessModifierMetadata.Public, "invalid", [], [], [], [], [], [], true, false);
        invalidType.MarkAsInvalid();
        var typeMatcher = new TypeMatcher(builtInTypes);

        var result = typeMatcher.Match(invalidType, invalidType);

        Assert.That(result, Is.EqualTo(InvalidMatch.Instance));
    }

    [Test]
    public void AliasToSameUnderlyingTypeReturnsSuccessfulMatch()
    {
        var builtInTypes = new BuiltInTypes();
        var alias = new AliasMetadata(null, AccessModifierMetadata.Public, "MyI32", [], builtInTypes.I32, false);
        var typeMatcher = new TypeMatcher(builtInTypes);

        var result = typeMatcher.Match(alias, builtInTypes.I32);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    public void UnderlyingTypeToAliasReturnsSuccessfulMatch()
    {
        var builtInTypes = new BuiltInTypes();
        var alias = new AliasMetadata(null, AccessModifierMetadata.Public, "MyI32", [], builtInTypes.I32, false);
        var typeMatcher = new TypeMatcher(builtInTypes);

        var result = typeMatcher.Match(builtInTypes.I32, alias);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    public void AliasToAliasSameUnderlyingTypeReturnsSuccessfulMatch()
    {
        var builtInTypes = new BuiltInTypes();
        var alias1 = new AliasMetadata(null, AccessModifierMetadata.Public, "MyI32", [], builtInTypes.I32, false);
        var alias2 = new AliasMetadata(null, AccessModifierMetadata.Public, "AnotherI32", [], builtInTypes.I32, false);
        var typeMatcher = new TypeMatcher(builtInTypes);

        var result = typeMatcher.Match(alias1, alias2);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    public void AliasToDifferentTypeReturnsFailedMatch()
    {
        var builtInTypes = new BuiltInTypes();
        var alias = new AliasMetadata(null, AccessModifierMetadata.Public, "MyI32", [], builtInTypes.I32, false);
        var typeMatcher = new TypeMatcher(builtInTypes);

        var result = typeMatcher.Match(alias, builtInTypes.Bool);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void AliasToConvertibleTypeReturnsConversionMatch()
    {
        var builtInTypes = new BuiltInTypes();
        var alias = new AliasMetadata(null, AccessModifierMetadata.Public, "MyI8", [], builtInTypes.I8, false);
        var typeMatcher = new TypeMatcher(builtInTypes);

        var result = typeMatcher.Match(alias, builtInTypes.I16);

        var implicitConversion = new ImplicitConversion(builtInTypes.I16);

        Assert.That(result, Is.EqualTo(implicitConversion));
    }

    [Test]
    public void AliasToLargerTypeReturnsConversionMatch()
    {
        var builtInTypes = new BuiltInTypes();
        var alias = new AliasMetadata(null, AccessModifierMetadata.Public, "MyI16", [], builtInTypes.I16, false);
        var typeMatcher = new TypeMatcher(builtInTypes);

        var result = typeMatcher.Match(alias, builtInTypes.I64);

        var implicitConversion = new ImplicitConversion(builtInTypes.I64);

        Assert.That(result, Is.EqualTo(implicitConversion));
    }

    [Test]
    public void AliasToSmallerTypeReturnsFailedMatch()
    {
        var builtInTypes = new BuiltInTypes();
        var alias = new AliasMetadata(null, AccessModifierMetadata.Public, "MyI64", [], builtInTypes.I64, false);
        var typeMatcher = new TypeMatcher(builtInTypes);

        var result = typeMatcher.Match(alias, builtInTypes.I8);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void AliasToInvalidTypeReturnsFailedMatch()
    {
        var builtInTypes = new BuiltInTypes();
        var invalidType = new TypeMetadata(null, AccessModifierMetadata.Public, "invalid", [], [], [], [], [], [], true, false);
        invalidType.MarkAsInvalid();
        var alias = new AliasMetadata(null, AccessModifierMetadata.Public, "MyI32", [], builtInTypes.I32, false);
        var typeMatcher = new TypeMatcher(builtInTypes);

        var result = typeMatcher.Match(alias, invalidType);

        Assert.That(result, Is.EqualTo(InvalidMatch.Instance));
    }

    [Test]
    public void InvalidAliasToTypeReturnsFailedMatch()
    {
        var builtInTypes = new BuiltInTypes();
        var invalidType = new TypeMetadata(null, AccessModifierMetadata.Public, "invalid", [], [], [], [], [], [], true, false);
        invalidType.MarkAsInvalid();
        var alias = new AliasMetadata(null, AccessModifierMetadata.Public, "MyI32", [], invalidType, false);
        var typeMatcher = new TypeMatcher(builtInTypes);

        var result = typeMatcher.Match(alias, builtInTypes.I32);

        Assert.That(result, Is.EqualTo(InvalidMatch.Instance));
    }

    [Test]
    public void AliasToNullReturnsFailedMatch()
    {
        var builtInTypes = new BuiltInTypes();
        var alias = new AliasMetadata(null, AccessModifierMetadata.Public, "MyI32", [], builtInTypes.I32, false);
        var typeMatcher = new TypeMatcher(builtInTypes);

        var result = typeMatcher.Match(alias, null);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void NullToAliasReturnsFailedMatch()
    {
        var builtInTypes = new BuiltInTypes();
        var alias = new AliasMetadata(null, AccessModifierMetadata.Public, "MyI32", [], builtInTypes.I32, false);
        var typeMatcher = new TypeMatcher(builtInTypes);

        var result = typeMatcher.Match(null, alias);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void DiscriminatedUnionSuccessfulMath()
    {
        var builtInTypes = new BuiltInTypes();
        var du = new DiscriminatedUnionMetadata(null, [
            builtInTypes.I32,
            builtInTypes.Null,
        ]);
        var typeMatcher = new TypeMatcher(builtInTypes);

        var result = typeMatcher.Match(builtInTypes.I32, du);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    public void DiscriminatedUnionOfDiscriminatedUnionSuccessfulMath()
    {
        var builtInTypes = new BuiltInTypes();
        var du = new DiscriminatedUnionMetadata(null, [
            builtInTypes.I32,
            new DiscriminatedUnionMetadata(null, [
                builtInTypes.I64,
                builtInTypes.Null,
            ]),
        ]);
        var typeMatcher = new TypeMatcher(builtInTypes);

        var result = typeMatcher.Match(builtInTypes.Null, du);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    public void DiscriminatedUnionFailedMath()
    {
        var builtInTypes = new BuiltInTypes();
        var du = new DiscriminatedUnionMetadata(null, [
            builtInTypes.I32,
            builtInTypes.Null,
        ]);
        var typeMatcher = new TypeMatcher(builtInTypes);

        var result = typeMatcher.Match(builtInTypes.I8, du);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void DiscriminatedUnionWithAliasSuccessfulMath()
    {
        var builtInTypes = new BuiltInTypes();
        var du = new DiscriminatedUnionMetadata(null, [
            new AliasMetadata(null, AccessModifierMetadata.Public, "MyI32", [], builtInTypes.I32, false),
            builtInTypes.Null,
        ]);
        var typeMatcher = new TypeMatcher(builtInTypes);

        var result = typeMatcher.Match(builtInTypes.I32, du);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }
}