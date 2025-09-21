using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseDirectiveTests
{
    private static SyntaxTree Parse(string code)
    {
        var diagnostics = new DiagnosticCollection();
        var lexer = new Lexer();
        var tokens = lexer.Tokenize(code, new LexerOptions(diagnostics.Lexer));

        return new Parser().Parse(tokens);
    }

    [Test]
    public void ParseIfDirectiveTest()
    {
        var tree = Parse(
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
        var tree = Parse(
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
        var tree = Parse(
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
        var tree = Parse(
            """
            public main(): void {
            #if D1
                print("D1");
            #else
                print("Empty");
            #endif
            }
            """);
        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(80, 7, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(80, 7, 2)),
                    [
                        new IfDirectiveNode(
                            new SourceSpan(new SourcePosition(22, 2, 1), new SourcePosition(78, 6, 7)),
                            "D1",
                            [
                                new ExpressionStatementNode(
                                    new SourceSpan(new SourcePosition(33, 3, 5), new SourcePosition(45, 3, 17)),
                                    new CallExpressionNode(
                                        new SourceSpan(new SourcePosition(33, 3, 5), new SourcePosition(44, 3, 16)),
                                        new MemberAccessExpressionNode(
                                            new SourceSpan(new SourcePosition(33, 3, 5), new SourcePosition(38, 3, 10)),
                                            "print"
                                        ),
                                        [
                                            LiteralExpressionNode.String(
                                                new SourceSpan(new SourcePosition(39, 3, 11), new SourcePosition(43, 3, 15)),
                                                "D1")
                                        ]
                                    )
                                )
                            ],
                            [
                                new ExpressionStatementNode(
                                    new SourceSpan(new SourcePosition(56, 5, 5), new SourcePosition(71, 5, 20)),
                                    new CallExpressionNode(
                                        new SourceSpan(new SourcePosition(56, 5, 5), new SourcePosition(70, 5, 19)),
                                        new MemberAccessExpressionNode(
                                            new SourceSpan(new SourcePosition(56, 5, 5), new SourcePosition(61, 5, 10)),
                                            "print"
                                        ),
                                        [
                                            LiteralExpressionNode.String(
                                                new SourceSpan(new SourcePosition(62, 5, 11), new SourcePosition(69, 5, 18)),
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