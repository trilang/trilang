using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseTypeTests
{
    [Test]
    public void ParseTypeTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            public type Point { }
            """);

        var expected = new SyntaxTree([
            new TypeDeclarationNode(
                AccessModifier.Public,
                "Point",
                [],
                []
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseTypeMissingTypeKeywordTest()
    {
        var parser = new Parser();
        const string code = """
                            public Point { }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseTypeMissingNameTest()
    {
        var parser = new Parser();
        const string code = """
                            public type { }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseTypeMissingOpenBraceTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseTypeMissingCloseBraceTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseFieldsTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            public type Point {
                private x: i32;
                private y: i32;
            }
            """);

        var expected = new SyntaxTree([
            new TypeDeclarationNode(
                AccessModifier.Public,
                "Point",
                [
                    new FieldDeclarationNode(AccessModifier.Private, "x", TypeNode.Create("i32")),
                    new FieldDeclarationNode(AccessModifier.Private, "y", TypeNode.Create("i32")),
                ],
                []
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseFieldMissingNameTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                private : i32;
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseFieldMissingColonTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                private x i32;
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseFieldMissingTypeTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                private x: ;
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseFieldMissingSemiColonTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                private x: i32
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseMethodsTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            public type Point {
                public toString(): string { }

                public distance(other: Point): f32 { }
            }
            """);

        var expected = new SyntaxTree([
            new TypeDeclarationNode(
                AccessModifier.Public,
                "Point",
                [],
                [
                    new MethodDeclarationNode(
                        AccessModifier.Public,
                        "toString",
                        [],
                        TypeNode.Create("string"),
                        new BlockStatementNode([])
                    ),
                    new MethodDeclarationNode(
                        AccessModifier.Public,
                        "distance",
                        [new ParameterNode("other", TypeNode.Create("Point"))],
                        TypeNode.Create("f32"),
                        new BlockStatementNode([])
                    )
                ]
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseMethodMissingNameTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                public (): string { }
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseMethodMissingOpenParenTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                public toString): string { }
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseMethodMissingCloseParenTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                public toString(: string { }
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseMethodMissingColonTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                public toString() string { }
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseMethodMissingReturnTypeTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                public toString(): { }
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseMethodMissingOpenBraceTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                public toString(): string }
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseMethodMissingCloseBraceTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                public toString(): string {
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseMethodMissingCommaTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                public toString(a: i32 b: i32): string { }
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseMethodMissingParameterColonTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                public toString(a i32): string { }
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }

    [Test]
    public void ParseMethodMissingParameterTypeTest()
    {
        var parser = new Parser();
        const string code = """
                            public type Point {
                                public toString(a: ): string { }
                            }
                            """;

        Assert.Throws<ParseException>(() => parser.Parse(code));
    }
}