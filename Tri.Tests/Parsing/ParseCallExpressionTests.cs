using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseCallExpressionTests
{
    private static readonly SourceFile file = new SourceFile("test.tri");

    private static (SyntaxTree, DiagnosticCollection) Parse(string code)
    {
        var diagnostics = new DiagnosticCollection();
        var lexer = new Lexer();
        var lexerOptions = new LexerOptions(new LexerDiagnosticReporter(diagnostics, file));
        var tokens = lexer.Tokenize(code, lexerOptions);
        var parser = new Parser();
        var parserOptions = new ParserOptions(file, new ParserDiagnosticReporter(diagnostics, file));
        var tree = parser.Parse(tokens, parserOptions);

        return (tree, diagnostics);
    }

    [Test]
    public void ParseCallStatementTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                print("Hello, World!");
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(51, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(51, 3, 2)),
                    [
                        new ExpressionStatementNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(49, 2, 28)),
                            new CallExpressionNode(
                                new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(48, 2, 27)),
                                new MemberAccessExpressionNode(
                                    new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(31, 2, 10)),
                                    "print"
                                ),
                                [
                                    LiteralExpressionNode.String(
                                        new SourceSpan(new SourcePosition(32, 2, 11), new SourcePosition(47, 2, 26)),
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
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseCallStatementMultipleParamsTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                sum(1, 2, 3);
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(41, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(41, 3, 2)),
                    [
                        new ExpressionStatementNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(39, 2, 18)),
                            new CallExpressionNode(
                                new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(38, 2, 17)),
                                new MemberAccessExpressionNode(
                                    new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(29, 2, 8)),
                                    "sum"
                                ),
                                [
                                    LiteralExpressionNode.Integer(
                                        new SourceSpan(new SourcePosition(30, 2, 9), new SourcePosition(31, 2, 10)),
                                        1
                                    ),
                                    LiteralExpressionNode.Integer(
                                        new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(34, 2, 13)),
                                        2
                                    ),
                                    LiteralExpressionNode.Integer(
                                        new SourceSpan(new SourcePosition(36, 2, 15), new SourcePosition(37, 2, 16)),
                                        3
                                    ),
                                ])
                        )
                    ]
                )
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseCallExpressionMultipleParamsTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var x: i32 = 1 + sum(1, 2, 3);
            }
            """);

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(58, 3, 2)),
                AccessModifier.Public,
                "main",
                [],
                new TypeNode(
                    new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(58, 3, 2)),
                    [
                        new VariableDeclarationNode(
                            new SourceSpan(new SourcePosition(26, 2, 5), new SourcePosition(56, 2, 35)),
                            "x",
                            new TypeNode(new SourceSpan(new SourcePosition(33, 2, 12), new SourcePosition(36, 2, 15)), "i32"),
                            new BinaryExpressionNode(
                                new SourceSpan(new SourcePosition(39, 2, 18), new SourcePosition(55, 2, 34)),
                                BinaryExpressionKind.Addition,
                                LiteralExpressionNode.Integer(
                                    new SourceSpan(new SourcePosition(39, 2, 18), new SourcePosition(40, 2, 19)),
                                    1
                                ),
                                new CallExpressionNode(
                                    new SourceSpan(new SourcePosition(43, 2, 22), new SourcePosition(55, 2, 34)),
                                    new MemberAccessExpressionNode(
                                        new SourceSpan(new SourcePosition(43, 2, 22), new SourcePosition(46, 2, 25)),
                                        "sum"
                                    ),
                                    [
                                        LiteralExpressionNode.Integer(
                                            new SourceSpan(new SourcePosition(47, 2, 26), new SourcePosition(48, 2, 27)),
                                            1
                                        ),
                                        LiteralExpressionNode.Integer(
                                            new SourceSpan(new SourcePosition(50, 2, 29), new SourcePosition(51, 2, 30)),
                                            2
                                        ),
                                        LiteralExpressionNode.Integer(
                                            new SourceSpan(new SourcePosition(53, 2, 32), new SourcePosition(54, 2, 33)),
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
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }
}