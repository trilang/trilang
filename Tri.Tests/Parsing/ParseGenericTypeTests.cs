using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseGenericTypeTests
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
    public void ParseGenericTypeTest()
    {
        var (tree, diagnostics) = Parse("public type List<T> { }");

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(23, 1, 24)),
                AccessModifier.Public,
                "List",
                [new TypeRefNode(new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(18, 1, 19)), "T")],
                [],
                [],
                [],
                []
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseGenericTypeMissingTypeTest()
    {
        var (tree, diagnostics) = Parse("public type List<> { }");

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(22, 1, 23)),
                AccessModifier.Public,
                "List",
                [
                    new FakeTypeNode(
                        new SourcePosition(17, 1, 18).ToSpan(),
                        "<>_0"
                    )
                ],
                [],
                [],
                [],
                []
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(17, 1, 18).ToSpan()),
            "Expected a type.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseGenericTypeMissingSecondTypeTest()
    {
        var (tree, diagnostics) = Parse("public type List<T,> { }");

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(24, 1, 25)),
                AccessModifier.Public,
                "List",
                [
                    new TypeRefNode(
                        new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(18, 1, 19)),
                        "T"
                    ),
                    new FakeTypeNode(
                        new SourcePosition(19, 1, 20).ToSpan(),
                        "<>_0"
                    )
                ],
                [],
                [],
                [],
                []
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(19, 1, 20).ToSpan()),
            "Expected a type.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseGenericTypeMissingGreaterTest()
    {
        var (tree, diagnostics) = Parse("public type List<T { }");

        var expected = new SyntaxTree(file, [
            new TypeDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(22, 1, 23)),
                AccessModifier.Public,
                "List",
                [
                    new TypeRefNode(
                        new SourceSpan(new SourcePosition(17, 1, 18), new SourcePosition(18, 1, 19)),
                        "T"
                    )
                ],
                [],
                [],
                [],
                []
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(19, 1, 20).ToSpan()),
            "Expected '>'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseAliasToGenericTypeTest()
    {
        var (tree, diagnostics) = Parse("public type T = List<i32, bool>;");

        var expected = new SyntaxTree(file, [
            new AliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(32, 1, 33)),
                AccessModifier.Public,
                "T",
                [],
                new GenericTypeRefNode(
                    new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(31, 1, 32)),
                    "List",
                    [new TypeRefNode(new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(24, 1, 25)), "i32"), new TypeRefNode(new SourceSpan(new SourcePosition(26, 1, 27), new SourcePosition(30, 1, 31)), "bool")]
                )
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseNestedGenericTypeAliasTest()
    {
        var (tree, diagnostics) = Parse("public type T = List<i32, List<bool>>;");
        var expected = new SyntaxTree(file, [
            new AliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(38, 1, 39)),
                AccessModifier.Public,
                "T",
                [],
                new GenericTypeRefNode(
                    new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(37, 1, 38)),
                    "List",
                    [
                        new TypeRefNode(new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(24, 1, 25)), "i32"),
                        new GenericTypeRefNode(new SourceSpan(new SourcePosition(26, 1, 27), new SourcePosition(36, 1, 37)), "List", [new TypeRefNode(new SourceSpan(new SourcePosition(31, 1, 32), new SourcePosition(35, 1, 36)), "bool")])
                    ]
                )
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseAliasToGenericTypeMissingTypeTest()
    {
        var (tree, diagnostics) = Parse("public type T = List<>;");

        var expected = new SyntaxTree(file, [
            new AliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(23, 1, 24)),
                AccessModifier.Public,
                "T",
                [],
                new GenericTypeRefNode(
                    new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(22, 1, 23)),
                    "List",
                    [
                        new FakeTypeNode(
                            new SourcePosition(21, 1, 22).ToSpan(),
                            "<>_0"
                        )
                    ]
                )
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(21, 1, 22).ToSpan()),
            "Expected a type.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseAliasToGenericTypeMissingSecondTypeTest()
    {
        var (tree, diagnostics) = Parse("public type T = List<i32, >;");

        var expected = new SyntaxTree(file, [
            new AliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(28, 1, 29)),
                AccessModifier.Public,
                "T",
                [],
                new GenericTypeRefNode(
                    new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(27, 1, 28)),
                    "List",
                    [
                        new TypeRefNode(
                            new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(24, 1, 25)),
                            "i32"
                        ),
                        new FakeTypeNode(
                            new SourcePosition(26, 1, 27).ToSpan(),
                            "<>_0"
                        )
                    ]
                )
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(26, 1, 27).ToSpan()),
            "Expected a type.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseAliasToGenericTypeMissingCloseAngleBracketTest()
    {
        var (tree, diagnostics) = Parse("public type T = List<i32;");

        var expected = new SyntaxTree(file, [
            new AliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(25, 1, 26)),
                AccessModifier.Public,
                "T",
                [],
                new GenericTypeRefNode(
                    new SourceSpan(new SourcePosition(16, 1, 17), new SourcePosition(24, 1, 25)),
                    "List",
                    [
                        new TypeRefNode(
                            new SourceSpan(new SourcePosition(21, 1, 22), new SourcePosition(24, 1, 25)),
                            "i32"
                        ),
                    ]
                )
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(24, 1, 25).ToSpan()),
            "Expected '>'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseGenericTypeAliasTest()
    {
        var (tree, diagnostics) = Parse("public type T<T1, T2> = T1 | T2;");

        var expected = new SyntaxTree(file, [
            new AliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(32, 1, 33)),
                AccessModifier.Public,
                "T",
                [new TypeRefNode(new SourceSpan(new SourcePosition(14, 1, 15), new SourcePosition(16, 1, 17)), "T1"), new TypeRefNode(new SourceSpan(new SourcePosition(18, 1, 19), new SourcePosition(20, 1, 21)), "T2")],
                new DiscriminatedUnionNode([
                    new TypeRefNode(new SourceSpan(new SourcePosition(24, 1, 25), new SourcePosition(26, 1, 27)), "T1"),
                    new TypeRefNode(new SourceSpan(new SourcePosition(29, 1, 30), new SourcePosition(31, 1, 32)), "T2")
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseGenericTypeAliasMissingTypeTest()
    {
        var (tree, diagnostics) = Parse("public type T<> = T1 | T2;");

        var expected = new SyntaxTree(file, [
            new AliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(26, 1, 27)),
                AccessModifier.Public,
                "T",
                [
                    new FakeTypeNode(
                        new SourcePosition(14, 1, 15).ToSpan(),
                        "<>_0"
                    )
                ],
                new DiscriminatedUnionNode([
                    new TypeRefNode(
                        new SourceSpan(new SourcePosition(18, 1, 19), new SourcePosition(20, 1, 21)),
                        "T1"
                    ),
                    new TypeRefNode(
                        new SourceSpan(new SourcePosition(23, 1, 24), new SourcePosition(25, 1, 26)),
                        "T2"
                    )
                ])
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(14, 1, 15).ToSpan()),
            "Expected a type.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseGenericTypeAliasMissingSecondTypeTest()
    {
        var (tree, diagnostics) = Parse("public type T<T1, > = T1 | T2;");

        var expected = new SyntaxTree(file, [
            new AliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(30, 1, 31)),
                AccessModifier.Public,
                "T",
                [
                    new TypeRefNode(
                        new SourceSpan(new SourcePosition(14, 1, 15), new SourcePosition(16, 1, 17)),
                        "T1"
                    ),
                    new FakeTypeNode(
                        new SourcePosition(18, 1, 19).ToSpan(),
                        "<>_0"
                    )
                ],
                new DiscriminatedUnionNode([
                    new TypeRefNode(
                        new SourceSpan(new SourcePosition(22, 1, 23), new SourcePosition(24, 1, 25)),
                        "T1"
                    ),
                    new TypeRefNode(
                        new SourceSpan(new SourcePosition(27, 1, 28), new SourcePosition(29, 1, 30)),
                        "T2"
                    )
                ])
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(18, 1, 19).ToSpan()),
            "Expected a type.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseGenericTypeAliasMissingCloseAngleBracketTest()
    {
        var (tree, diagnostics) = Parse("public type T<T1, T2 = T1 | T2;");

        var expected = new SyntaxTree(file, [
            new AliasDeclarationNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(31, 1, 32)),
                AccessModifier.Public,
                "T",
                [
                    new TypeRefNode(
                        new SourceSpan(new SourcePosition(14, 1, 15), new SourcePosition(16, 1, 17)),
                        "T1"
                    ),
                    new TypeRefNode(
                        new SourceSpan(new SourcePosition(18, 1, 19), new SourcePosition(20, 1, 21)),
                        "T2"
                    )
                ],
                new DiscriminatedUnionNode([
                    new TypeRefNode(
                        new SourceSpan(new SourcePosition(23, 1, 24), new SourcePosition(25, 1, 26)),
                        "T1"
                    ),
                    new TypeRefNode(
                        new SourceSpan(new SourcePosition(28, 1, 29), new SourcePosition(30, 1, 31)),
                        "T2"
                    )
                ])
            )
        ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(21, 1, 22).ToSpan()),
            "Expected '>'.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }
}