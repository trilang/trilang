using Trilang;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseDirectiveTests
{
    [Test]
    public void ParseIfDirectiveTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            #if D1

            public type Type1 { }

            #endif
            """);
        var expected = new SyntaxTree([
            new IfDirectiveNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(37, 5, 7)),
                "D1",
                [
                    new TypeDeclarationNode(
                        new SourceSpan(new SourcePosition(8, 3, 1), new SourcePosition(29, 3, 22)),
                        AccessModifier.Public,
                        "Type1",
                        [],
                        [],
                        [],
                        [],
                        [])
                ],
                []
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseIfDirectiveWithElseTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            #if D1

            public type Type1 { }

            #else

            public type Type2 { }

            #endif
            """);
        var expected = new SyntaxTree([
            new IfDirectiveNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(67, 9, 7)),
                "D1",
                [
                    new TypeDeclarationNode(
                        new SourceSpan(new SourcePosition(8, 3, 1), new SourcePosition(29, 3, 22)),
                        AccessModifier.Public,
                        "Type1",
                        [],
                        [],
                        [],
                        [],
                        [])
                ],
                [
                    new TypeDeclarationNode(
                        new SourceSpan(new SourcePosition(38, 7, 1), new SourcePosition(59, 7, 22)),
                        AccessModifier.Public,
                        "Type2",
                        [],
                        [],
                        [],
                        [],
                        [])
                ]
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseNestedIfDirectiveTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            #if D1

            public type Type1 { }

            #if D2

            public type Type2 { }

            #endif

            public type Type3 { }

            #endif
            """);
        var expected = new SyntaxTree([
            new IfDirectiveNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(99, 13, 7)),
                "D1",
                [
                    new TypeDeclarationNode(
                        new SourceSpan(new SourcePosition(8, 3, 1), new SourcePosition(29, 3, 22)),
                        AccessModifier.Public,
                        "Type1",
                        [],
                        [],
                        [],
                        [],
                        []),
                    new IfDirectiveNode(
                        new SourceSpan(new SourcePosition(31, 5, 1), new SourcePosition(68, 9, 7)),
                        "D2",
                        [
                            new TypeDeclarationNode(
                                new SourceSpan(new SourcePosition(39, 7, 1), new SourcePosition(60, 7, 22)),
                                AccessModifier.Public,
                                "Type2",
                                [],
                                [],
                                [],
                                [],
                                [])
                        ],
                        []),
                    new TypeDeclarationNode(
                        new SourceSpan(new SourcePosition(70, 11, 1), new SourcePosition(91, 11, 22)),
                        AccessModifier.Public,
                        "Type3",
                        [],
                        [],
                        [],
                        [],
                        []),
                ],
                []
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void ParseIfDirectiveStatementTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            function main(): void {
            #if D1
                print("D1");
            #else
                print("Empty");
            #endif
            }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(82, 7, 2)),
                "main",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(21, 1, 22)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(82, 7, 2)),
                    [
                        new IfDirectiveNode(
                            new SourceSpan(new SourcePosition(24, 2, 1), new SourcePosition(80, 6, 7)),
                            "D1",
                            [
                                new ExpressionStatementNode(
                                    new SourceSpan(new SourcePosition(35, 3, 5), new SourcePosition(47, 3, 17)),
                                    new CallExpressionNode(
                                        new SourceSpan(new SourcePosition(35, 3, 5), new SourcePosition(46, 3, 16)),
                                        new MemberAccessExpressionNode(
                                            new SourceSpan(new SourcePosition(35, 3, 5), new SourcePosition(40, 3, 10)),
                                            "print"
                                        ),
                                        [
                                            LiteralExpressionNode.String(
                                                new SourceSpan(new SourcePosition(41, 3, 11), new SourcePosition(45, 3, 15)),
                                                "D1")
                                        ]
                                    )
                                )
                            ],
                            [
                                new ExpressionStatementNode(
                                    new SourceSpan(new SourcePosition(58, 5, 5), new SourcePosition(73, 5, 20)),
                                    new CallExpressionNode(
                                        new SourceSpan(new SourcePosition(58, 5, 5), new SourcePosition(72, 5, 19)),
                                        new MemberAccessExpressionNode(
                                            new SourceSpan(new SourcePosition(58, 5, 5), new SourcePosition(63, 5, 10)),
                                            "print"
                                        ),
                                        [
                                            LiteralExpressionNode.String(
                                                new SourceSpan(new SourcePosition(64, 5, 11), new SourcePosition(71, 5, 18)),
                                                "Empty")
                                        ]
                                    )
                                )
                            ]
                        )
                    ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }
}