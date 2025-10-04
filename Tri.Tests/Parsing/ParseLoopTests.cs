using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Compilation;

namespace Tri.Tests.Parsing;

public class ParseLoopTests
{
    private static readonly SourceFile file = new SourceFile("test.tri", "test.tri");

    private static (SyntaxTree, DiagnosticCollection) Parse(string code)
    {
        var diagnostics = new DiagnosticCollection();
        diagnostics.SwitchFile(file);

        var lexer = new Lexer();
        var tokens = lexer.Tokenize(code, new LexerOptions(diagnostics.Lexer));
        var parser = new Parser();
        var tree = parser.Parse(tokens, new ParserOptions(diagnostics.Parser));

        return (tree, diagnostics);
    }

    [Test]
    public void ParseWhileTest()
    {
        var (tree, diagnostics) = Parse(
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
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseWhileMissingOpenParenTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(x: i32): void {
                while x > 0) {
                }
            }
            """);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(54, 4, 2)),
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
                    new SourceSpan(new SourcePosition(26, 1, 27), new SourcePosition(54, 4, 2)),
                    [
                        new WhileNode(
                            new SourceSpan(new SourcePosition(32, 2, 5), new SourcePosition(52, 3, 6)),
                            new BinaryExpressionNode(
                                new SourceSpan(new SourcePosition(38, 2, 11), new SourcePosition(43, 2, 16)),
                                BinaryExpressionKind.GreaterThan,
                                new MemberAccessExpressionNode(
                                    new SourceSpan(new SourcePosition(38, 2, 11), new SourcePosition(39, 2, 12)),
                                    "x"
                                ),
                                LiteralExpressionNode.Integer(
                                    new SourceSpan(new SourcePosition(42, 2, 15), new SourcePosition(43, 2, 16)),
                                    0
                                )
                            ),
                            new BlockStatementNode(
                                new SourceSpan(new SourcePosition(45, 2, 18), new SourcePosition(52, 3, 6))
                            )
                        )
                    ]
                )
            ),
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticIds.P0001_MissingToken,
            DiagnosticSeverity.Error,
            file,
            new SourcePosition(38, 2, 11).ToSpan(),
            "Expected '('.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseWhileMissingConditionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(x: i32): void {
                while (;) {
                }
            }
            """);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(51, 4, 2)),
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
                    new SourceSpan(new SourcePosition(26, 1, 27), new SourcePosition(51, 4, 2)),
                    [
                        new WhileNode(
                            new SourceSpan(new SourcePosition(32, 2, 5), new SourcePosition(49, 3, 6)),
                            new FakeExpressionNode(
                                new SourceSpan(new SourcePosition(39, 2, 12), new SourcePosition(40, 2, 13))
                            ),
                            new BlockStatementNode(
                                new SourceSpan(new SourcePosition(42, 2, 15), new SourcePosition(49, 3, 6))
                            )
                        )
                    ]
                )
            ),
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticIds.P0009_ExpectedExpression,
            DiagnosticSeverity.Error,
            file,
            new SourceSpan(new SourcePosition(39, 2, 12), new SourcePosition(40, 2, 13)),
            "Expected an expression.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseWhileMissingCloseParenTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public test(x: i32): void {
                while (x > 0 {
                }
            }
            """);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(54, 4, 2)),
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
                    new SourceSpan(new SourcePosition(26, 1, 27), new SourcePosition(54, 4, 2)),
                    [
                        new WhileNode(
                            new SourceSpan(new SourcePosition(32, 2, 5), new SourcePosition(52, 3, 6)),
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
                                new SourceSpan(new SourcePosition(45, 2, 18), new SourcePosition(52, 3, 6))
                            )
                        )
                    ]
                )
            ),
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticIds.P0001_MissingToken,
            DiagnosticSeverity.Error,
            file,
            new SourcePosition(45, 2, 18).ToSpan(),
            "Expected ')'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseWhileMissingBlockTest()
    {
        var source = """
                     public test(x: i32): void {
                         while (x > 0)
                            ;
                     }
                     """;

        var (tree, diagnostics) = Parse(source);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(56, 4, 2)),
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
                    new SourceSpan(new SourcePosition(26, 1, 27), new SourcePosition(56, 4, 2)),
                    [
                        new WhileNode(
                            new SourceSpan(new SourcePosition(32, 2, 5), new SourcePosition(56, 4, 2)),
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
                                new SourceSpan(new SourcePosition(53, 3, 8), new SourcePosition(56, 4, 2)),
                                [
                                    new FakeStatementNode(
                                        new SourcePosition(53, 3, 8).ToSpan()
                                    )
                                ]
                            )
                        )
                    ]
                )
            ),
        ]);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticIds.P0001_MissingToken,
                DiagnosticSeverity.Error,
                file,
                new SourcePosition(53, 3, 8).ToSpan(),
                "Expected '{'."),
            new Diagnostic(
                DiagnosticIds.P0004_ExpectedStatement,
                DiagnosticSeverity.Error,
                file,
                new SourcePosition(53, 3, 8).ToSpan(),
                "Expected a statement."),
            new Diagnostic(
                DiagnosticIds.P0001_MissingToken,
                DiagnosticSeverity.Error,
                file,
                new SourcePosition(56, 4, 2).ToSpan(),
                "Expected '}'."),
        };

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseBreakTest()
    {
        var (tree, diagnostics) = Parse(
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
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseContinueTest()
    {
        var (tree, diagnostics) = Parse(
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
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }
}