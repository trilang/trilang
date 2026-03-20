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
            namespace Test1;

            public main(): void {
                print("Hello, World!");
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                FunctionDeclarationNode.Create(
                    default,
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(default, ["void"]),
                    new BlockStatementNode(
                        default,
                        [
                            new ExpressionStatementNode(
                                default,
                                new CallExpressionNode(
                                    default,
                                    new MemberAccessExpressionNode(
                                        default,
                                        "print"
                                    ),
                                    [
                                        LiteralExpressionNode.String(
                                            default,
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
            namespace Test1;

            public main(): void {
                sum(1, 2, 3);
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                FunctionDeclarationNode.Create(
                    default,
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(default, ["void"]),
                    new BlockStatementNode(
                        default,
                        [
                            new ExpressionStatementNode(
                                default,
                                new CallExpressionNode(
                                    default,
                                    new MemberAccessExpressionNode(
                                        default,
                                        "sum"
                                    ),
                                    [
                                        LiteralExpressionNode.Integer(
                                            default,
                                            1
                                        ),
                                        LiteralExpressionNode.Integer(
                                            default,
                                            2
                                        ),
                                        LiteralExpressionNode.Integer(
                                            default,
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
            namespace Test1;

            public main(): void {
                var x: i32 = 1 + sum(1, 2, 3);
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                FunctionDeclarationNode.Create(
                    default,
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(default, ["void"]),
                    new BlockStatementNode(
                        default,
                        [
                            new VariableDeclarationNode(
                                default,
                                "x",
                                new TypeRefNode(default, ["i32"]),
                                new BinaryExpressionNode(
                                    default,
                                    BinaryExpressionKind.Addition,
                                    LiteralExpressionNode.Integer(
                                        default,
                                        1
                                    ),
                                    new CallExpressionNode(
                                        default,
                                        new MemberAccessExpressionNode(
                                            default,
                                            "sum"
                                        ),
                                        [
                                            LiteralExpressionNode.Integer(
                                                default,
                                                1
                                            ),
                                            LiteralExpressionNode.Integer(
                                                default,
                                                2
                                            ),
                                            LiteralExpressionNode.Integer(
                                                default,
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