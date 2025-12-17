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
        var (tree, diagnostics) = Parse("public test): void { }");

        var expected = new SyntaxTree(file, [
            new FunctionDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(22, 1, 23)),
                AccessModifier.Public,
                "test",
                [],
                new TypeRefNode(
                    new SourceSpan(new SourcePosition(14, 1, 15), new SourcePosition(18, 1, 19)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(19, 1, 20), new SourcePosition(22, 1, 23))
                )
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourceSpan(new SourcePosition(11, 1, 12), new SourcePosition(11, 1, 12))),
            "Expected '('."
        );

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMissingParamColonFunctionTest()
    {
        var (tree, diagnostics) = Parse("public test(x i32): void { }");

        var expected = new SyntaxTree(file, [
            new FunctionDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(28, 1, 29)),
                AccessModifier.Public,
                "test",
                [
                    new ParameterNode(
                        new SourceSpan(new SourcePosition(12, 1, 13), new SourcePosition(17, 1, 18)),
                        "x",
                        new TypeRefNode(
                            new SourceSpan(new SourcePosition(14, 1, 15), new SourcePosition(17, 1, 18)),
                            "i32"
                        )
                    )
                ],
                new TypeRefNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(24, 1, 25)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(25, 1, 26), new SourcePosition(28, 1, 29))
                )
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(14, 1, 15).ToSpan()),
            "Expected ':'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMissingParamTypeFunctionTest()
    {
        var (tree, diagnostics) = Parse("public test(x: ): void { }");

        var expected = new SyntaxTree(file, [
            new FunctionDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(26, 1, 27)),
                AccessModifier.Public,
                "test",
                [
                    new ParameterNode(
                        new SourceSpan(new SourcePosition(12, 1, 13), new SourcePosition(15, 1, 16)),
                        "x",
                        new FakeTypeNode(
                            new SourcePosition(15, 1, 16).ToSpan(),
                            "<>_0"
                        )
                    )
                ],
                new TypeRefNode(
                    new SourceSpan(new SourcePosition(18, 1, 19), new SourcePosition(22, 1, 23)),
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
            new SourceLocation(file, new SourcePosition(15, 1, 16).ToSpan()),
            "Expected a type.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMissingCommaFunctionTest()
    {
        var (tree, diagnostics) = Parse("public test(x: i32 y: i32): void { }");

        var expected = new SyntaxTree(file, [
            new FunctionDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(36, 1, 37)),
                AccessModifier.Public,
                "test",
                [
                    new ParameterNode(
                        new SourceSpan(new SourcePosition(12, 1, 13), new SourcePosition(18, 1, 19)),
                        "x",
                        new TypeRefNode(
                            new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(18, 1, 19)),
                            "i32"
                        )
                    ),
                    new ParameterNode(
                        new SourceSpan(new SourcePosition(19, 1, 20), new SourcePosition(25, 1, 26)),
                        "y",
                        new TypeRefNode(
                            new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(25, 1, 26)),
                            "i32"
                        )
                    ),
                ],
                new TypeRefNode(
                    new SourceSpan(new SourcePosition(28, 1, 29), new SourcePosition(32, 1, 33)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(33, 1, 34), new SourcePosition(36, 1, 37))
                )
            ),
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(19, 1, 20).ToSpan()),
            "Expected ','."
        );

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMissingCloseParenFunctionTest()
    {
        var (tree, diagnostics) = Parse("public test( : void { return; }");

        var expected = new SyntaxTree(file, [
            new FunctionDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(31, 1, 32)),
                AccessModifier.Public,
                "test",
                [],
                new TypeRefNode(
                    new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(31, 1, 32)),
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
                new SourceLocation(file, new SourcePosition(13, 1, 14).ToSpan()),
                "Expected ')'."),
        };

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseMissingReturnColonFunctionTest()
    {
        var (tree, diagnostics) = Parse("public test() void { return; }");

        var expected = new SyntaxTree(file, [
            new FunctionDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(30, 1, 31)),
                AccessModifier.Public,
                "test",
                [],
                new TypeRefNode(
                    new SourceSpan(new SourcePosition(14, 1, 15), new SourcePosition(18, 1, 19)),
                    "void"
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(19, 1, 20), new SourcePosition(30, 1, 31)),
                    [
                        new ReturnStatementNode(
                            new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(28, 1, 29))
                        )
                    ]
                )
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(14, 1, 15).ToSpan()),
            "Expected ':'."
        );

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMissingReturnTypeFunctionTest()
    {
        var (tree, diagnostics) = Parse("public test(): { return; }");

        var expected = new SyntaxTree(file, [
            new FunctionDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(26, 1, 27)),
                AccessModifier.Public,
                "test",
                [],
                new InterfaceNode(
                    new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(17, 1, 18)),
                    [],
                    []
                ),
                new BlockStatementNode(
                    new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(26, 1, 27)),
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
                new SourceLocation(file, new SourcePosition(17, 1, 18).ToSpan()),
                "Expected '}'."
            ),
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(17, 1, 18).ToSpan()),
                "Expected '{'."
            )
        };

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseMissingBlockInFunctionTest()
    {
        var (tree, diagnostics) = Parse("public test(): void");

        var expected = new SyntaxTree(file, [
            new FunctionDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(19, 1, 20)),
                AccessModifier.Public,
                "test",
                [],
                new TypeRefNode(
                    new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)),
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
                new SourceLocation(file, new SourcePosition(19, 1, 20).ToSpan()),
                "Expected '{'."
            ),
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(19, 1, 20).ToSpan()),
                "Expected '}'."
            )
        };

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseEmptyFunctionTest()
    {
        var (tree, diagnostics) = Parse("public test(): void { }");

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(23, 1, 24)),
                AccessModifier.Public,
                "test",
                [],
                new TypeRefNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(19, 1, 20)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(23, 1, 24)))
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseSingleParameterFunctionTest()
    {
        var (tree, diagnostics) = Parse("public test(x: i32): void { }");

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(29, 1, 30)),
                AccessModifier.Public,
                "test",
                [
                    new ParameterNode(new SourceSpan(new SourcePosition(12, 1, 13), new SourcePosition(18, 1, 19)), "x", new TypeRefNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(18, 1, 19)), "i32")),
                ],
                new TypeRefNode(new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(25, 1, 26)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(26, 1, 27), new SourcePosition(29, 1, 30)))
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseMultipleParametersFunctionTest()
    {
        var (tree, diagnostics) = Parse("public test(x: i32, y: i32, z: i32): void { }");

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(45, 1, 46)),
                AccessModifier.Public,
                "test",
                [
                    new ParameterNode(new SourceSpan(new SourcePosition(12, 1, 13), new SourcePosition(18, 1, 19)), "x", new TypeRefNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(18, 1, 19)), "i32")),
                    new ParameterNode(new SourceSpan(new SourcePosition(20, 1, 21), new SourcePosition(26, 1, 27)), "y", new TypeRefNode(new SourceSpan(new SourcePosition(23, 1, 24), new SourcePosition(26, 1, 27)), "i32")),
                    new ParameterNode(new SourceSpan(new SourcePosition(28, 1, 29), new SourcePosition(34, 1, 35)), "z", new TypeRefNode(new SourceSpan(new SourcePosition(31, 1, 32), new SourcePosition(34, 1, 35)), "i32")),
                ],
                new TypeRefNode(new SourceSpan(new SourcePosition(37, 1, 38), new SourcePosition(41, 1, 42)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(42, 1, 43), new SourcePosition(45, 1, 46)))
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseArrayTypeTest()
    {
        var (tree, diagnostics) = Parse("public test(x: i32[]): void { }");

        var expected = new SyntaxTree(file, [
            FunctionDeclarationNode.Create(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(31, 1, 32)),
                AccessModifier.Public,
                "test",
                [new ParameterNode(new SourceSpan(new SourcePosition(12, 1, 13), new SourcePosition(20, 1, 21)), "x", new ArrayTypeNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(20, 1, 21)), new TypeRefNode(new SourceSpan(new SourcePosition(15, 1, 16), new SourcePosition(18, 1, 19)), "i32")))],
                new TypeRefNode(new SourceSpan(new SourcePosition(23, 1, 24), new SourcePosition(27, 1, 28)), "void"),
                new BlockStatementNode(new SourceSpan(new SourcePosition(28, 1, 29), new SourcePosition(31, 1, 32)))
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }
}