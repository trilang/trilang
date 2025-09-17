using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang;

namespace Tri.Tests.Parsing;

public class ParseLoopTests
{
    [Test]
    public void ParseWhileTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            public test(x: i32): void {
                while (x > 0) {
                }
            }
            """);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(55, 4, 2)),
                AccessModifier.Public,
                "test",
                [
                    new ParameterNode(
                        new SourceSpan(new SourcePosition(12, 1, 13), new SourcePosition(18, 1, 19)),
                        "x",
                        new TypeNode(
                            new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(18, 1, 19)),
                            "i32"
                        )
                    )
                ],
                new TypeNode(new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(25, 1, 26)), "void"),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(26, 1, 27), new SourcePosition(55, 4, 2)),
                    [
                        new WhileNode(
                            new SourceSpan(new SourcePosition(32, 2, 5), new SourcePosition(53, 3, 6)),
                            new BinaryExpressionNode(
                                new SourceSpan(
                                    new SourcePosition(39, 2, 12), new SourcePosition(44, 2, 17)
                                ),
                                BinaryExpressionKind.GreaterThan,
                                new MemberAccessExpressionNode(
                                    new SourceSpan(new SourcePosition(39, 2, 12), new SourcePosition(40, 2, 13)),
                                    "x"
                                ),
                                LiteralExpressionNode.Integer(
                                    new SourceSpan(new SourcePosition(43, 2, 16), new SourcePosition(44, 2, 17)),
                                    0
                                )
                            ),
                            new BlockStatementNode(
                                new SourceSpan(new SourcePosition(46, 2, 19), new SourcePosition(53, 3, 6))
                            )
                        )
                    ]
                )
            ),
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseWhileMissingOpenParenTest()
    {
        var parser = new Parser();
        var source = """
                     public test(x: i32): void {
                         while x > 0) {
                         }
                     }
                     """;

        Assert.Throws<ParseException>(() => parser.Parse(source));
    }

    [Test]
    public void ParseWhileMissingConditionTest()
    {
        var parser = new Parser();
        var source = """
                     public test(x: i32): void {
                         while (;) {
                         }
                     }
                     """;

        Assert.Throws<ParseException>(() => parser.Parse(source));
    }

    [Test]
    public void ParseWhileMissingCloseParenTest()
    {
        var parser = new Parser();
        var source = """
                     public test(x: i32): void {
                         while (x > 0 {
                         }
                     }
                     """;

        Assert.Throws<ParseException>(() => parser.Parse(source));
    }

    [Test]
    public void ParseWhileMissingBlockTest()
    {
        var parser = new Parser();
        var source = """
                     public test(x: i32): void {
                         while (x > 0)
                            ;
                     }
                     """;

        Assert.Throws<ParseException>(() => parser.Parse(source));
    }

    [Test]
    public void ParseBreakTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            public test(x: i32): void {
                while (x > 0) {
                    break;
                }
            }
            """);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(70, 5, 2)),
                AccessModifier.Public,
                "test",
                [
                    new ParameterNode(
                        new SourceSpan(new SourcePosition(12, 1, 13), new SourcePosition(18, 1, 19)),
                        "x",
                        new TypeNode(
                            new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(18, 1, 19)),
                            "i32"
                        )
                    )
                ],
                new TypeNode(
                    new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(25, 1, 26)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(26, 1, 27), new SourcePosition(70, 5, 2)),
                    [
                        new WhileNode(
                            new SourceSpan(new SourcePosition(32, 2, 5), new SourcePosition(68, 4, 6)),
                            new BinaryExpressionNode(
                                new SourceSpan(new SourcePosition(39, 2, 12), new SourcePosition(44, 2, 17)),
                                BinaryExpressionKind.GreaterThan,
                                new MemberAccessExpressionNode(
                                    new SourceSpan(new SourcePosition(39, 2, 12), new SourcePosition(40, 2, 13)),
                                    "x"
                                ),
                                LiteralExpressionNode.Integer(
                                    new SourceSpan(new SourcePosition(43, 2, 16), new SourcePosition(44, 2, 17)),
                                    0
                                )
                            ),
                            new BlockStatementNode(
                                new SourceSpan(new SourcePosition(46, 2, 19), new SourcePosition(68, 4, 6)),
                                [
                                    new BreakNode(
                                        new SourceSpan(new SourcePosition(56, 3, 9), new SourcePosition(62, 3, 15))
                                    )
                                ]
                            )
                        )
                    ])
            ),
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseContinueTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            public test(x: i32): void {
                while (x > 0) {
                    continue;
                }
            }
            """);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(73, 5, 2)),
                AccessModifier.Public,
                "test",
                [
                    new ParameterNode(
                        new SourceSpan(new SourcePosition(12, 1, 13), new SourcePosition(18, 1, 19)),
                        "x",
                        new TypeNode(
                            new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(18, 1, 19)),
                            "i32"
                        )
                    )
                ],
                new TypeNode(
                    new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(25, 1, 26)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(26, 1, 27), new SourcePosition(73, 5, 2)),
                    [
                        new WhileNode(
                            new SourceSpan(new SourcePosition(32, 2, 5), new SourcePosition(71, 4, 6)),
                            new BinaryExpressionNode(
                                new SourceSpan(new SourcePosition(39, 2, 12), new SourcePosition(44, 2, 17)),
                                BinaryExpressionKind.GreaterThan,
                                new MemberAccessExpressionNode(
                                    new SourceSpan(new SourcePosition(39, 2, 12), new SourcePosition(40, 2, 13)),
                                    "x"
                                ),
                                LiteralExpressionNode.Integer(
                                    new SourceSpan(new SourcePosition(43, 2, 16), new SourcePosition(44, 2, 17)),
                                    0
                                )
                            ),
                            new BlockStatementNode(
                                new SourceSpan(new SourcePosition(46, 2, 19), new SourcePosition(71, 4, 6)),
                                [
                                    new ContinueNode(
                                        new SourceSpan(new SourcePosition(56, 3, 9), new SourcePosition(65, 3, 18))
                                    )
                                ]
                            )
                        )
                    ])
            ),
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }
}