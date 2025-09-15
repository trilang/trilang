using Trilang;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseCallExpressionTests
{
    [Test]
    public void ParseCallStatementTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                print("Hello, World!");
            }
            """);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(53, 3, 2)),
                "main",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(53, 3, 2)),
                    [
                        new ExpressionStatementNode(
                            new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(51, 2, 28)),
                            new CallExpressionNode(
                                new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(50, 2, 27)),
                                new MemberAccessExpressionNode(
                                    new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(33, 2, 10)),
                                    "print"
                                ),
                                [
                                    LiteralExpressionNode.String(
                                        new SourceSpan(new SourcePosition(34, 2, 11), new SourcePosition(49, 2, 26)),
                                        "Hello, World!"
                                    )
                                ]
                            )
                        )
                    ]
                )
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseCallStatementMultipleParamsTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                sum(1, 2, 3);
            }
            """);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(43, 3, 2)),
                "main",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(43, 3, 2)),
                    [
                        new ExpressionStatementNode(
                            new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(41, 2, 18)),
                            new CallExpressionNode(
                                new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(40, 2, 17)),
                                new MemberAccessExpressionNode(
                                    new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(31, 2, 8)),
                                    "sum"
                                ),
                                [
                                    LiteralExpressionNode.Integer(
                                        new SourceSpan(new SourcePosition(32, 2, 9), new SourcePosition(33, 2, 10)),
                                        1
                                    ),
                                    LiteralExpressionNode.Integer(
                                        new SourceSpan(new SourcePosition(35, 2, 12), new SourcePosition(36, 2, 13)),
                                        2
                                    ),
                                    LiteralExpressionNode.Integer(
                                        new SourceSpan(new SourcePosition(38, 2, 15), new SourcePosition(39, 2, 16)),
                                        3
                                    ),
                                ])
                        )
                    ]
                )
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseCallExpressionMultipleParamsTest()
    {
        var parse = new Parser();
        var tree = parse.Parse(
            """
            function main(): void {
                var x: i32 = 1 + sum(1, 2, 3);
            }
            """);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(60, 3, 2)),
                "main",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(60, 3, 2)),
                    [
                        new VariableDeclarationNode(
                            new SourceSpan(new SourcePosition(28, 2, 5), new SourcePosition(58, 2, 35)),
                            "x",
                            new TypeNode(new SourceSpan(new SourcePosition(35, 2, 12), new SourcePosition(38, 2, 15)), "i32"),
                            new BinaryExpressionNode(
                                new SourceSpan(new SourcePosition(41, 2, 18), new SourcePosition(57, 2, 34)),
                                BinaryExpressionKind.Addition,
                                LiteralExpressionNode.Integer(
                                    new SourceSpan(new SourcePosition(41, 2, 18), new SourcePosition(42, 2, 19)),
                                    1
                                ),
                                new CallExpressionNode(
                                    new SourceSpan(new SourcePosition(45, 2, 22), new SourcePosition(57, 2, 34)),
                                    new MemberAccessExpressionNode(
                                        new SourceSpan(new SourcePosition(45, 2, 22), new SourcePosition(48, 2, 25)),
                                        "sum"
                                    ),
                                    [
                                        LiteralExpressionNode.Integer(
                                            new SourceSpan(new SourcePosition(49, 2, 26), new SourcePosition(50, 2, 27)),
                                            1
                                        ),
                                        LiteralExpressionNode.Integer(
                                            new SourceSpan(new SourcePosition(52, 2, 29), new SourcePosition(53, 2, 30)),
                                            2
                                        ),
                                        LiteralExpressionNode.Integer(
                                            new SourceSpan(new SourcePosition(55, 2, 32), new SourcePosition(56, 2, 33)),
                                            3
                                        ),
                                    ])
                            )
                        )
                    ]
                )
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }
}