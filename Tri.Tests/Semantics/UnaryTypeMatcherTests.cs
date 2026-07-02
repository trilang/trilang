using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Semantics.TypeMatchers;
using static Trilang.Semantics.Model.UnaryExpressionKind;

namespace Tri.Tests.Semantics;

public class UnaryTypeMatcherTests
{
    private static IEnumerable<object[]> GetIntegerTypes()
    {
        var builtInTypes = new BuiltInTypes();

        yield return [builtInTypes, builtInTypes.I8];
        yield return [builtInTypes, builtInTypes.I16];
        yield return [builtInTypes, builtInTypes.I32];
        yield return [builtInTypes, builtInTypes.I64];
        yield return [builtInTypes, builtInTypes.U8];
        yield return [builtInTypes, builtInTypes.U16];
        yield return [builtInTypes, builtInTypes.U32];
        yield return [builtInTypes, builtInTypes.U64];
    }

    private static IEnumerable<object[]> GetFloatTypes()
    {
        var builtInTypes = new BuiltInTypes();

        yield return [builtInTypes, builtInTypes.F32];
        yield return [builtInTypes, builtInTypes.F64];
    }

    private static IEnumerable<object[]> GetUnaryExpressionKinds()
    {
        yield return [UnaryMinus];
        yield return [UnaryPlus];
        yield return [LogicalNot];
        yield return [BitwiseNot];
        yield return [AddressOf];
        yield return [Dereference];
        yield return [Unknown];
    }

    [Test]
    [TestCaseSource(nameof(GetUnaryExpressionKinds))]
    public void UnaryExpressionWithNullOperandReturnsFailed(UnaryExpressionKind kind)
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(kind, null);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    [TestCaseSource(nameof(GetUnaryExpressionKinds))]
    public void UnaryExpressionWithInvalidOperandReturnsInvalid(UnaryExpressionKind kind)
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);
        var invalidType = TypeMetadata.Invalid("invalid");

        var result = matcher.Match(kind, invalidType);

        Assert.That(result, Is.EqualTo(InvalidMatch.Instance));
    }

    [Test]
    [TestCaseSource(nameof(GetIntegerTypes))]
    public void UnaryMinusWithIntegerTypeReturnsSuccess(BuiltInTypes builtInTypes, ITypeMetadata type)
    {
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(UnaryMinus, type);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    [TestCaseSource(nameof(GetFloatTypes))]
    public void UnaryMinusWithFloatTypeReturnsSuccess(BuiltInTypes builtInTypes, ITypeMetadata type)
    {
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(UnaryMinus, type);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    public void UnaryMinusWithBoolReturnsFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(UnaryMinus, builtInTypes.Bool);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void UnaryMinusWithStringReturnsFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(UnaryMinus, builtInTypes.String);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void UnaryMinusWithVoidReturnsFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(UnaryMinus, builtInTypes.Void);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void UnaryMinusWithPointerReturnsFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);
        var pointer = new PointerMetadata(null, builtInTypes.I32);

        var result = matcher.Match(UnaryMinus, pointer);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    [TestCaseSource(nameof(GetIntegerTypes))]
    public void UnaryPlusWithIntegerTypeReturnsSuccess(BuiltInTypes builtInTypes, ITypeMetadata type)
    {
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(UnaryPlus, type);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    [TestCaseSource(nameof(GetFloatTypes))]
    public void UnaryPlusWithFloatTypeReturnsSuccess(BuiltInTypes builtInTypes, ITypeMetadata type)
    {
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(UnaryPlus, type);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    public void UnaryPlusWithBoolReturnsFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(UnaryPlus, builtInTypes.Bool);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void UnaryPlusWithStringReturnsFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(UnaryPlus, builtInTypes.String);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void LogicalNotWithBoolReturnsSuccess()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(LogicalNot, builtInTypes.Bool);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    public void LogicalNotWithI32ReturnsFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(LogicalNot, builtInTypes.I32);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void LogicalNotWithStringReturnsFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(LogicalNot, builtInTypes.String);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void LogicalNotWithVoidReturnsFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(LogicalNot, builtInTypes.Void);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void LogicalNotWithPointerReturnsFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);
        var pointer = new PointerMetadata(null, builtInTypes.I32);

        var result = matcher.Match(LogicalNot, pointer);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    [TestCaseSource(nameof(GetIntegerTypes))]
    public void BitwiseNotWithIntegerTypeReturnsSuccess(BuiltInTypes builtInTypes, ITypeMetadata type)
    {
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(BitwiseNot, type);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    [TestCaseSource(nameof(GetFloatTypes))]
    public void BitwiseNotWithFloatTypeReturnsFailed(BuiltInTypes builtInTypes, ITypeMetadata type)
    {
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(BitwiseNot, type);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void BitwiseNotWithBoolReturnsFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(BitwiseNot, builtInTypes.Bool);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void BitwiseNotWithStringReturnsFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(BitwiseNot, builtInTypes.String);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void BitwiseNotWithVoidReturnsFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(BitwiseNot, builtInTypes.Void);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void BitwiseNotWithPointerReturnsFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);
        var pointer = new PointerMetadata(null, builtInTypes.I32);

        var result = matcher.Match(BitwiseNot, pointer);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    [TestCaseSource(nameof(GetIntegerTypes))]
    public void AddressOfWithIntegerTypeReturnsSuccess(BuiltInTypes builtInTypes, ITypeMetadata type)
    {
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(AddressOf, type);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    public void AddressOfWithBoolReturnsSuccess()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(AddressOf, builtInTypes.Bool);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    public void AddressOfWithStringReturnsSuccess()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(AddressOf, builtInTypes.String);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    public void AddressOfWithVoidReturnsSuccess()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(AddressOf, builtInTypes.Void);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    public void AddressOfWithPointerReturnsSuccess()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);
        var pointer = new PointerMetadata(null, builtInTypes.I32);

        var result = matcher.Match(AddressOf, pointer);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    public void AddressOfWithFloatTypeReturnsSuccess()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(AddressOf, builtInTypes.F32);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    public void DereferenceWithPointerToI32ReturnsSuccess()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);
        var pointer = new PointerMetadata(null, builtInTypes.I32);

        var result = matcher.Match(Dereference, pointer);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    public void DereferenceWithPointerToVoidReturnsSuccess()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);
        var pointer = new PointerMetadata(null, builtInTypes.Void);

        var result = matcher.Match(Dereference, pointer);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    public void DereferenceWithPointerToPointerReturnsSuccess()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);
        var innerPointer = new PointerMetadata(null, builtInTypes.I32);
        var pointer = new PointerMetadata(null, innerPointer);

        var result = matcher.Match(Dereference, pointer);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    public void DereferenceWithI32ReturnsFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(Dereference, builtInTypes.I32);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void DereferenceWithBoolReturnsFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(Dereference, builtInTypes.Bool);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void DereferenceWithStringReturnsFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(Dereference, builtInTypes.String);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void DereferenceWithFloatReturnsFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);

        var result = matcher.Match(Dereference, builtInTypes.F32);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void UnaryMinusWithAliasToI32ReturnsSuccess()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);
        var alias = new AliasMetadata(null, AccessModifierMetadata.Public, "MyI32", [], builtInTypes.I32, false);

        var result = matcher.Match(UnaryMinus, alias);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    public void UnaryPlusWithAliasToFloatReturnsSuccess()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);
        var alias = new AliasMetadata(null, AccessModifierMetadata.Public, "MyFloat", [], builtInTypes.F64, false);

        var result = matcher.Match(UnaryPlus, alias);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    public void LogicalNotWithAliasToBoolReturnsSuccess()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);
        var alias = new AliasMetadata(null, AccessModifierMetadata.Public, "MyBool", [], builtInTypes.Bool, false);

        var result = matcher.Match(LogicalNot, alias);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    public void BitwiseNotWithAliasToI32ReturnsSuccess()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);
        var alias = new AliasMetadata(null, AccessModifierMetadata.Public, "MyInt", [], builtInTypes.I64, false);

        var result = matcher.Match(BitwiseNot, alias);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    public void UnaryMinusWithAliasToStringReturnsFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);
        var alias = new AliasMetadata(null, AccessModifierMetadata.Public, "MyString", [], builtInTypes.String, false);

        var result = matcher.Match(UnaryMinus, alias);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void UnaryPlusWithAliasToStringReturnsFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);
        var alias = new AliasMetadata(null, AccessModifierMetadata.Public, "MyString", [], builtInTypes.String, false);

        var result = matcher.Match(UnaryPlus, alias);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void LogicalNotWithAliasToI32ReturnsFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);
        var alias = new AliasMetadata(null, AccessModifierMetadata.Public, "MyInt", [], builtInTypes.I32, false);

        var result = matcher.Match(LogicalNot, alias);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void BitwiseNotWithAliasToFloatReturnsFailed()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);
        var alias = new AliasMetadata(null, AccessModifierMetadata.Public, "MyFloat", [], builtInTypes.F32, false);

        var result = matcher.Match(BitwiseNot, alias);

        Assert.That(result, Is.EqualTo(FailedMatch.Instance));
    }

    [Test]
    public void DereferenceWithAliasToPointerReturnsSuccess()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);
        var pointer = new PointerMetadata(null, builtInTypes.I32);
        var alias = new AliasMetadata(null, AccessModifierMetadata.Public, "MyPointer", [], pointer, false);

        var result = matcher.Match(Dereference, alias);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    public void AddressOfWithAliasReturnsSuccess()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);
        var alias = new AliasMetadata(null, AccessModifierMetadata.Public, "MyType", [], builtInTypes.String, false);

        var result = matcher.Match(AddressOf, alias);

        Assert.That(result, Is.EqualTo(SuccessMatch.Instance));
    }

    [Test]
    public void UnaryMinusWithInvalidAliasReturnsInvalid()
    {
        var builtInTypes = new BuiltInTypes();
        var typeMatcher = new TypeMatcher(builtInTypes);
        var matcher = new UnaryTypeMatcher(typeMatcher);
        var invalidAlias = new AliasMetadata(null, AccessModifierMetadata.Public, "InvalidAlias", [], builtInTypes.I32, false);
        invalidAlias.MarkAsInvalid();

        var result = matcher.Match(UnaryMinus, invalidAlias);

        Assert.That(result, Is.EqualTo(InvalidMatch.Instance));
    }
}