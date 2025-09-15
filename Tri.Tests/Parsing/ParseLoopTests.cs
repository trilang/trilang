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
            function test(x: i32): void {
                while (x > 0) {
                }
            }
            """);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(57, 4, 2)),
                "test",
                [
                    new ParameterNode(
                        new SourceSpan(new SourcePosition(14, 1, 15), new SourcePosition(20, 1, 21)),
                        "x",
                        new TypeNode(
                            new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(20, 1, 21)),
                            "i32"
                        )
                    )
                ],
                new TypeNode(new SourceSpan(new SourcePosition(23, 1, 24), new SourcePosition(27, 1, 28)), "void"),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(28, 1, 29), new SourcePosition(57, 4, 2)),
                    [
                        new WhileNode(
                            new SourceSpan(new SourcePosition(34, 2, 5), new SourcePosition(55, 3, 6)),
                            new BinaryExpressionNode(
                                new SourceSpan(
                                    new SourcePosition(41, 2, 12), new SourcePosition(46, 2, 17)
                                ),
                                BinaryExpressionKind.GreaterThan,
                                new MemberAccessExpressionNode(
                                    new SourceSpan(new SourcePosition(41, 2, 12), new SourcePosition(42, 2, 13)),
                                    "x"
                                ),
                                LiteralExpressionNode.Integer(
                                    new SourceSpan(new SourcePosition(45, 2, 16), new SourcePosition(46, 2, 17)),
                                    0
                                )
                            ),
                            new BlockStatementNode(
                                new SourceSpan(new SourcePosition(48, 2, 19), new SourcePosition(55, 3, 6))
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
                     function test(x: i32): void {
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
                     function test(x: i32): void {
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
                     function test(x: i32): void {
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
                     function test(x: i32): void {
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
            function test(x: i32): void {
                while (x > 0) {
                    break;
                }
            }
            """);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(72, 5, 2)),
                "test",
                [
                    new ParameterNode(
                        new SourceSpan(new SourcePosition(14, 1, 15), new SourcePosition(20, 1, 21)),
                        "x",
                        new TypeNode(
                            new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(20, 1, 21)),
                            "i32"
                        )
                    )
                ],
                new TypeNode(
                    new SourceSpan(new SourcePosition(23, 1, 24), new SourcePosition(27, 1, 28)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(28, 1, 29), new SourcePosition(72, 5, 2)),
                    [
                        new WhileNode(
                            new SourceSpan(new SourcePosition(34, 2, 5), new SourcePosition(70, 4, 6)),
                            new BinaryExpressionNode(
                                new SourceSpan(new SourcePosition(41, 2, 12), new SourcePosition(46, 2, 17)),
                                BinaryExpressionKind.GreaterThan,
                                new MemberAccessExpressionNode(
                                    new SourceSpan(new SourcePosition(41, 2, 12), new SourcePosition(42, 2, 13)),
                                    "x"
                                ),
                                LiteralExpressionNode.Integer(
                                    new SourceSpan(new SourcePosition(45, 2, 16), new SourcePosition(46, 2, 17)),
                                    0
                                )
                            ),
                            new BlockStatementNode(
                                new SourceSpan(new SourcePosition(48, 2, 19), new SourcePosition(70, 4, 6)),
                                [
                                    new BreakNode(
                                        new SourceSpan(new SourcePosition(58, 3, 9), new SourcePosition(64, 3, 15))
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
            function test(x: i32): void {
                while (x > 0) {
                    continue;
                }
            }
            """);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(75, 5, 2)),
                "test",
                [
                    new ParameterNode(
                        new SourceSpan(new SourcePosition(14, 1, 15), new SourcePosition(20, 1, 21)),
                        "x",
                        new TypeNode(
                            new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(20, 1, 21)),
                            "i32"
                        )
                    )
                ],
                new TypeNode(
                    new SourceSpan(new SourcePosition(23, 1, 24), new SourcePosition(27, 1, 28)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(28, 1, 29), new SourcePosition(75, 5, 2)),
                    [
                        new WhileNode(
                            new SourceSpan(new SourcePosition(34, 2, 5), new SourcePosition(73, 4, 6)),
                            new BinaryExpressionNode(
                                new SourceSpan(new SourcePosition(41, 2, 12), new SourcePosition(46, 2, 17)),
                                BinaryExpressionKind.GreaterThan,
                                new MemberAccessExpressionNode(
                                    new SourceSpan(new SourcePosition(41, 2, 12), new SourcePosition(42, 2, 13)),
                                    "x"
                                ),
                                LiteralExpressionNode.Integer(
                                    new SourceSpan(new SourcePosition(45, 2, 16), new SourcePosition(46, 2, 17)),
                                    0
                                )
                            ),
                            new BlockStatementNode(
                                new SourceSpan(new SourcePosition(48, 2, 19), new SourcePosition(73, 4, 6)),
                                [
                                    new ContinueNode(
                                        new SourceSpan(new SourcePosition(58, 3, 9), new SourcePosition(67, 3, 18))
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