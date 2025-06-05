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
                "D1",
                [
                    new TypeDeclarationNode(
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

        Assert.That(tree, Is.EqualTo(expected));
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
                "D1",
                [
                    new TypeDeclarationNode(
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

        Assert.That(tree, Is.EqualTo(expected));
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
                "D1",
                [
                    new TypeDeclarationNode(
                        AccessModifier.Public,
                        "Type1",
                        [],
                        [],
                        [],
                        [],
                        []),
                    new IfDirectiveNode(
                        "D2",
                        [
                            new TypeDeclarationNode(
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

        Assert.That(tree, Is.EqualTo(expected));
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
                "main",
                [],
                new TypeNode("void"),
                new BlockStatementNode([
                    new IfDirectiveNode(
                        "D1",
                        [
                            new ExpressionStatementNode(
                                new CallExpressionNode(
                                    new MemberAccessExpressionNode("print"),
                                    [LiteralExpressionNode.String("D1")]
                                )
                            )
                        ],
                        [
                            new ExpressionStatementNode(
                                new CallExpressionNode(
                                    new MemberAccessExpressionNode("print"),
                                    [LiteralExpressionNode.String("Empty")]
                                )
                            )
                        ]
                    )
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }
}