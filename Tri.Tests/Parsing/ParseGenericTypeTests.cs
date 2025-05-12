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
                AccessModifier.Public,
                "List",
                [new TypeNode("T")],
                [],
                [],
                [],
                []
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
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
    public void ParseGenericTypeAliasTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public type T = List<i32, bool>;");
        var expected = new SyntaxTree([
            new TypeAliasDeclarationNode(
                AccessModifier.Public,
                "T",
                new GenericTypeNode("List", [new TypeNode("i32"), new TypeNode("bool")])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseNestedGenericTypeAliasTest()
    {
        var parser = new Parser();
        var tree = parser.Parse("public type T = List<i32, List<bool>>;");
        var expected = new SyntaxTree([
            new TypeAliasDeclarationNode(
                AccessModifier.Public,
                "T",
                new GenericTypeNode(
                    "List",
                    [
                        new TypeNode("i32"),
                        new GenericTypeNode("List", [new TypeNode("bool")])
                    ]
                )
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseGenericTypeAliasMissingTypeTest()
    {
        var parser = new Parser();
        const string code = "public type T = List<>;";

        Assert.That(
            () => parser.Parse(code),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected a type argument."));
    }

    [Test]
    public void ParseGenericTypeAliasMissingSecondTypeTest()
    {
        var parser = new Parser();
        const string code = "public type T = List<i32, >;";

        Assert.That(
            () => parser.Parse(code),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected a type argument."));
    }

    [Test]
    public void ParseGenericTypeAliasMissingCloseAngleBracketTest()
    {
        var parser = new Parser();
        const string code = "public type T = List<i32;";

        Assert.That(
            () => parser.Parse(code),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected a close angle bracket."));
    }
}