using Trilang;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseGenericTypeTests
{
    [Test]
    public void ParseGenericTypeTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public type List<T> { }");
        var expected = new SyntaxTree([
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(23, 1, 24)),
                AccessModifier.Public,
                "List",
                [new TypeNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(18, 1, 19)), "T")],
                [],
                [],
                [],
                []
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseGenericTypeMissingTypeTest()
    {
        var parser = new Parser();
        const string code = "public type List<> { }";

        Assert.That(
            () => parser.Parse(code),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected a type."));
    }

    [Test]
    public void ParseGenericTypeMissingSecondTypeTest()
    {
        var parser = new Parser();
        const string code = "public type List<T,> { }";

        Assert.That(
            () => parser.Parse(code),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected a type."));
    }

    [Test]
    public void ParseGenericTypeMissingGreaterTest()
    {
        var parser = new Parser();
        const string code = "public type List<T { }";

        Assert.That(
            () => parser.Parse(code),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected a greater than sign."));
    }

    [Test]
    public void ParseAliasToGenericTypeTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public type T = List<i32, bool>;");
        var expected = new SyntaxTree([
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(32, 1, 33)),
                AccessModifier.Public,
                "T",
                [],
                new GenericTypeNode(
                    new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(31, 1, 32)),
                    "List",
                    [new TypeNode(new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(24, 1, 25)), "i32"), new TypeNode(new SourceSpan(new SourcePosition(26, 1, 27), new SourcePosition(30, 1, 31)), "bool")]
                )
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseNestedGenericTypeAliasTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public type T = List<i32, List<bool>>;");
        var expected = new SyntaxTree([
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(38, 1, 39)),
                AccessModifier.Public,
                "T",
                [],
                new GenericTypeNode(
                    new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(37, 1, 38)),
                    "List",
                    [
                        new TypeNode(new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(24, 1, 25)), "i32"),
                        new GenericTypeNode(new SourceSpan(new SourcePosition(26, 1, 27), new SourcePosition(36, 1, 37)), "List", [new TypeNode(new SourceSpan(new SourcePosition(31, 1, 32), new SourcePosition(35, 1, 36)), "bool")])
                    ]
                )
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseAliasToGenericTypeMissingTypeTest()
    {
        var parser = new Parser();
        const string code = "public type T = List<>;";

        Assert.That(
            () => parser.Parse(code),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected a type argument."));
    }

    [Test]
    public void ParseAliasToGenericTypeMissingSecondTypeTest()
    {
        var parser = new Parser();
        const string code = "public type T = List<i32, >;";

        Assert.That(
            () => parser.Parse(code),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected a type argument."));
    }

    [Test]
    public void ParseAliasToGenericTypeMissingCloseAngleBracketTest()
    {
        var parser = new Parser();
        const string code = "public type T = List<i32;";

        Assert.That(
            () => parser.Parse(code),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected a close angle bracket."));
    }

    [Test]
    public void ParseGenericTypeAliasTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public type T<T1, T2> = T1 | T2;");
        var expected = new SyntaxTree([
            new TypeAliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(32, 1, 33)),
                AccessModifier.Public,
                "T",
                [new TypeNode(new SourceSpan(new SourcePosition(14, 1, 15), new SourcePosition(16, 1, 17)), "T1"), new TypeNode(new SourceSpan(new SourcePosition(18, 1, 19), new SourcePosition(20, 1, 21)), "T2")],
                new DiscriminatedUnionNode([
                    new TypeNode(new SourceSpan(new SourcePosition(24, 1, 25), new SourcePosition(26, 1, 27)), "T1"),
                    new TypeNode(new SourceSpan(new SourcePosition(29, 1, 30), new SourcePosition(31, 1, 32)), "T2")
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseGenericTypeAliasMissingTypeTest()
    {
        var parser = new Parser();
        const string code = "public type T<> = T1 | T2;";

        Assert.That(
            () => parser.Parse(code),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected a type."));
    }

    [Test]
    public void ParseGenericTypeAliasMissingSecondTypeTest()
    {
        var parser = new Parser();
        const string code = "public type T<T1, > = T1 | T2;";

        Assert.That(
            () => parser.Parse(code),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected a type."));
    }

    [Test]
    public void ParseGenericTypeAliasMissingCloseAngleBracketTest()
    {
        var parser = new Parser();
        const string code = "public type T<T1, T2 = T1 | T2;";

        Assert.That(
            () => parser.Parse(code),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected a greater than sign."));
    }
}