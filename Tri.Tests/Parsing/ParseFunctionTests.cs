using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseFunctionTests
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
    public void ParseMissingOpenParenFunctionTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test): void { }");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new FunctionDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "test",
                    [],
                    new TypeRefNode(
                        default,
                        "void"
                    ),
                    new BlockStatementNode(default)
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(29, 3, 12).ToSpan()),
            "Expected '('."
        );

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMissingParamColonFunctionTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test(x i32): void { }");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new FunctionDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "test",
                    [
                        new ParameterNode(
                            default,
                            "x",
                            new TypeRefNode(
                                default,
                                "i32"
                            )
                        )
                    ],
                    new TypeRefNode(
                        default,
                        "void"
                    ),
                    new BlockStatementNode(default)
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(32, 3, 15).ToSpan()),
            "Expected ':'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMissingParamTypeFunctionTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test(x: ): void { }");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new FunctionDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "test",
                    [
                        new ParameterNode(
                            default,
                            "x",
                            new FakeTypeNode(
                                default,
                                "<>_0"
                            )
                        )
                    ],
                    new TypeRefNode(
                        default,
                        "void"
                    ),
                    new BlockStatementNode(
                        new SourceSpan(new SourcePosition(23, 1, 24), new SourcePosition(26, 1, 27))
                    )
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(33, 3, 16).ToSpan()),
            "Expected a type.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMissingCommaFunctionTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test(x: i32 y: i32): void { }");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new FunctionDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "test",
                    [
                        new ParameterNode(
                            default,
                            "x",
                            new TypeRefNode(
                                default,
                                "i32"
                            )
                        ),
                        new ParameterNode(
                            default,
                            "y",
                            new TypeRefNode(
                                default,
                                "i32"
                            )
                        ),
                    ],
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(default)
                ),
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(37, 3, 20).ToSpan()),
            "Expected ','."
        );

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMissingCloseParenFunctionTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test( : void { return; }");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new FunctionDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "test",
                    [],
                    new TypeRefNode(
                        default,
                        "void"
                    ),
                    new BlockStatementNode(
                        default,
                        [
                            new ReturnStatementNode(
                                new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(29, 1, 30))
                            )
                        ]
                    )
                )
            ]);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(31, 3, 14).ToSpan()),
                "Expected ')'."),
        };

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseMissingReturnColonFunctionTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test() void { return; }");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new FunctionDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "test",
                    [],
                    new TypeRefNode(
                        default,
                        "void"
                    ),
                    new BlockStatementNode(
                        default,
                        [
                            new ReturnStatementNode(default)
                        ]
                    )
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(32, 3, 15).ToSpan()),
            "Expected ':'."
        );

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMissingReturnTypeFunctionTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test(): { return; }");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new FunctionDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "test",
                    [],
                    new InterfaceNode(
                        default,
                        [],
                        []
                    ),
                    new BlockStatementNode(
                        default,
                        [
                            new ReturnStatementNode(
                                new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(24, 1, 25))
                            )
                        ]
                    )
                )
            ]);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(35, 3, 18).ToSpan()),
                "Expected '}'."
            ),
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(35, 3, 18).ToSpan()),
                "Expected '{'."
            )
        };

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseMissingBlockInFunctionTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test(): void");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                new FunctionDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "test",
                    [],
                    new TypeRefNode(
                        default,
                        "void"
                    ),
                    new BlockStatementNode(
                        new SourcePosition(19, 1, 20).ToSpan()
                    )
                )
            ]);

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(37, 3, 20).ToSpan()),
                "Expected '{'."
            ),
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(37, 3, 20).ToSpan()),
                "Expected '}'."
            )
        };

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseEmptyFunctionTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test(): void { }");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                FunctionDeclarationNode.Create(
                    default,
                    AccessModifier.Public,
                    "test",
                    [],
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(default)
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseSingleParameterFunctionTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test(x: i32): void { }");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                FunctionDeclarationNode.Create(
                    default,
                    AccessModifier.Public,
                    "test",
                    [
                        new ParameterNode(default, "x", new TypeRefNode(default, "i32")),
                    ],
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(default)
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseMultipleParametersFunctionTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test(x: i32, y: i32, z: i32): void { }");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                FunctionDeclarationNode.Create(
                    default,
                    AccessModifier.Public,
                    "test",
                    [
                        new ParameterNode(default, "x", new TypeRefNode(default, "i32")),
                        new ParameterNode(default, "y", new TypeRefNode(default, "i32")),
                        new ParameterNode(default, "z", new TypeRefNode(default, "i32")),
                    ],
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(default)
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseArrayTypeTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test(x: i32[]): void { }");

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [],
            [
                FunctionDeclarationNode.Create(
                    default,
                    AccessModifier.Public,
                    "test",
                    [new ParameterNode(default, "x", new ArrayTypeNode(default, new TypeRefNode(default, "i32")))],
                    new TypeRefNode(default, "void"),
                    new BlockStatementNode(default)
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }
}