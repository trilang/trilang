using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseMemberAccessTests
{
    [Test]
    public void ParseArrayAccessTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function test(x: i32[]): void {
                var a: i32 = x[0];
            }
            """);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "test",
                [new ParameterNode("x", new ArrayTypeNode(new TypeNode("i32")))],
                new TypeNode("void"),
                new BlockStatementNode([
                    new VariableDeclarationStatementNode(
                        "a",
                        new TypeNode("i32"),
                        new ArrayAccessExpressionNode(
                            new MemberAccessExpressionNode("x"),
                            LiteralExpressionNode.Number(0)
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseArrayAccessMissingCloseTest()
    {
        var parse = new Parser();
        const string code =
            """
            function test(x: i32[]): void {
                var a: i32 = x[0;
            }
            """;

        Assert.Throws<ParseException>(() => parse.Parse(code));
    }

    [Test]
    public void ParseSetArrayTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            function test(): void {
                x[0] = 1;
            }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "test",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new ExpressionStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.Assignment,
                            new ArrayAccessExpressionNode(
                                new MemberAccessExpressionNode("x"),
                                LiteralExpressionNode.Number(0)
                            ),
                            LiteralExpressionNode.Number(1)
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseSetNestedArrayTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            function test(): void {
                a.b[0].c = 1;
            }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "test",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new ExpressionStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.Assignment,
                            new MemberAccessExpressionNode(
                                new ArrayAccessExpressionNode(
                                    new MemberAccessExpressionNode(
                                        new MemberAccessExpressionNode("a"),
                                        "b"
                                    ),
                                    LiteralExpressionNode.Number(0)
                                ),
                                "c"
                            ),
                            LiteralExpressionNode.Number(1)
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseMultipleArrayAccessTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            function main(): void {
                return a[0][1];
            }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new ArrayAccessExpressionNode(
                            new ArrayAccessExpressionNode(
                                new MemberAccessExpressionNode("a"),
                                LiteralExpressionNode.Number(0)
                            ),
                            LiteralExpressionNode.Number(1)
                        )
                    )
                ]))
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseMultipleMemberAccessTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                return a.b.c;
            }
            """);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new MemberAccessExpressionNode(
                            new MemberAccessExpressionNode(
                                new MemberAccessExpressionNode("a"),
                                "b"
                            ),
                            "c"
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseMultipleMemberAccessMissingExpressionTest()
    {
        var parse = new Parser();
        const string code =
            """
            function main(): void {
                return a.b.;
            }
            """;

        Assert.That(
            () => parse.Parse(code),
            Throws.TypeOf<ParseException>()
                .And.Message.EqualTo("Expected an identifier."));
    }

    [Test]
    public void ParseMemberAccessNestedCallTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                return a.b().c;
            }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new MemberAccessExpressionNode(
                            new CallExpressionNode(
                                new MemberAccessExpressionNode(
                                    new MemberAccessExpressionNode("a"),
                                    "b"
                                ),
                                []
                            ),
                            "c"
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseMultipleCallsTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            function main(): void {
                f(1)(2);
            }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new ExpressionStatementNode(
                        new CallExpressionNode(
                            new CallExpressionNode(
                                new MemberAccessExpressionNode("f"),
                                [LiteralExpressionNode.Number(1)]
                            ),
                            [LiteralExpressionNode.Number(2)]
                        )
                    )
                ]))
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseMemberAccessAfterCtorTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            function main(): void {
                return new Test().a;
            }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new MemberAccessExpressionNode(
                            new NewObjectExpressionNode(
                                new TypeNode("Test"),
                                []
                            ),
                            "a"
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseMemberAccessAfterNewArrayTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            function main(): void {
                return new i32[0].size;
            }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new MemberAccessExpressionNode(
                            new NewArrayExpressionNode(
                                new ArrayTypeNode(new TypeNode("i32")),
                                LiteralExpressionNode.Number(0)
                            ),
                            "size"
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseCallExpWithBinaryExpTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            function main(): void {
                return 1.toString();
            }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new CallExpressionNode(
                            new MemberAccessExpressionNode(
                                LiteralExpressionNode.Number(1),
                                "toString"
                            ),
                            []
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseCallExpWithParenExpTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            function main(): void {
                return 1 + 2.toString();
            }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "main",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new ReturnStatementNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.Addition,
                            LiteralExpressionNode.Number(1),
                            new CallExpressionNode(
                                new MemberAccessExpressionNode(
                                    LiteralExpressionNode.Number(2),
                                    "toString"
                                ),
                                []
                            )
                        )
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }
}