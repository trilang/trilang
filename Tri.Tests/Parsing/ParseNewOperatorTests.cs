using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseNewOperatorTests
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
    public void ParseNewOperatorTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                var p: Point = new Point();
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
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(default, [
                        new VariableDeclarationNode(
                            default,
                            "p",
                            new TypeRefNode(default, "Point"),
                            new NewObjectExpressionNode(default, new TypeRefNode(default, "Point"), [])
                        )
                    ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseNewOperatorWithParametersTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                var p: Point = new Point(1, 2);
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
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(default, [
                        new VariableDeclarationNode(
                            default,
                            "p",
                            new TypeRefNode(default, "Point"),
                            new NewObjectExpressionNode(
                                default,
                                new TypeRefNode(default, "Point"),
                                [
                                    LiteralExpressionNode.Integer(default, 1),
                                    LiteralExpressionNode.Integer(default, 2)
                                ]
                            )
                        )
                    ])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseNewOperatorMissingTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                var p: Point = new ();
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new FunctionDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(
                        default,
                        "void"
                    ),
                    new BlockStatementNode(
                        default,
                        [
                            new VariableDeclarationNode(
                                default,
                                "p",
                                new TypeRefNode(
                                    default,
                                    "Point"
                                ),
                                new NewObjectExpressionNode(
                                    default,
                                    new FakeTypeNode(
                                        default,
                                        "<>_0"
                                    ),
                                    []
                                )
                            )
                        ]
                    )
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(63, 4, 24).ToSpan()),
            "Expected a type.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseNewOperatorMissingArgumentTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                var p: Point = new Point(1, );
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new FunctionDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(
                        default,
                        "void"
                    ),
                    new BlockStatementNode(
                        default,
                        [
                            new VariableDeclarationNode(
                                default,
                                "p",
                                new TypeRefNode(
                                    default,
                                    "Point"
                                ),
                                new NewObjectExpressionNode(
                                    default,
                                    new TypeRefNode(
                                        default,
                                        "Point"
                                    ),
                                    [
                                        LiteralExpressionNode.Integer(
                                            default,
                                            1
                                        ),
                                        new FakeExpressionNode(
                                            new SourcePosition(54, 2, 33).ToSpan()
                                        )
                                    ]
                                )
                            )
                        ]
                    )
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0009ExpectedExpression,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(72, 4, 33).ToSpan()),
            "Expected an expression.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseNewOperatorMissingCloseParenTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                var p: Point = new Point(;
            }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new FunctionDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(
                        default,
                        "void"
                    ),
                    new BlockStatementNode(
                        default,
                        [
                            new VariableDeclarationNode(
                                default,
                                "p",
                                new TypeRefNode(
                                    default,
                                    "Point"
                                ),
                                new NewObjectExpressionNode(
                                    default,
                                    new TypeRefNode(
                                        default,
                                        "Point"
                                    ),
                                    []
                                )
                            )
                        ]
                    )
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(69, 4, 30).ToSpan()),
            "Expected ')'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }
}