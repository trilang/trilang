using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseNamespaceTests
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
    public void ParseNamespaceTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test;

            public main(): void { }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(15, 1, 16)),
                ["Test"]),
            [],
            [
                new FunctionDeclarationNode(
                    new SourceSpan(new SourcePosition(17, 3, 1), new SourcePosition(40, 3, 24)),
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(
                        new SourceSpan(new SourcePosition(32, 3, 16), new SourcePosition(36, 3, 20)),
                        "void"),
                    new BlockStatementNode(
                        new SourceSpan(new SourcePosition(37, 3, 21), new SourcePosition(40, 3, 24)),
                        [])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseMultiPartNamespaceTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test.SubNamespace.SubSubNamespace;

            public main(): void { }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(44, 1, 45)),
                ["Test", "SubNamespace", "SubSubNamespace"]),
            [],
            [
                new FunctionDeclarationNode(
                    new SourceSpan(new SourcePosition(46, 3, 1), new SourcePosition(69, 3, 24)),
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(
                        new SourceSpan(new SourcePosition(61, 3, 16), new SourcePosition(65, 3, 20)),
                        "void"),
                    new BlockStatementNode(
                        new SourceSpan(new SourcePosition(66, 3, 21), new SourcePosition(69, 3, 24)),
                        [])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseUseNamespaceTest()
    {
        var (tree, diagnostics) = Parse(
            """
            use Test;

            public main(): void { }
            """);

        var expected = new SyntaxTree(
            file,
            null,
            [
                new UseNode(
                    new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(9, 1, 10)),
                    ["Test"])
            ],
            [
                new FunctionDeclarationNode(
                    new SourceSpan(new SourcePosition(11, 3, 1), new SourcePosition(34, 3, 24)),
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(
                        new SourceSpan(new SourcePosition(26, 3, 16), new SourcePosition(30, 3, 20)),
                        "void"),
                    new BlockStatementNode(
                        new SourceSpan(new SourcePosition(31, 3, 21), new SourcePosition(34, 3, 24)),
                        [])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseUseMultipartNamespaceTest()
    {
        var (tree, diagnostics) = Parse(
            """
            use Test.SubNamespace.SubSubNamespace;

            public main(): void { }
            """);

        var expected = new SyntaxTree(
            file,
            null,
            [
                new UseNode(
                    new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(38, 1, 39)),
                    ["Test", "SubNamespace", "SubSubNamespace"])
            ],
            [
                new FunctionDeclarationNode(
                    new SourceSpan(new SourcePosition(40, 3, 1), new SourcePosition(63, 3, 24)),
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(
                        new SourceSpan(new SourcePosition(55, 3, 16), new SourcePosition(59, 3, 20)),
                        "void"),
                    new BlockStatementNode(
                        new SourceSpan(new SourcePosition(60, 3, 21), new SourcePosition(63, 3, 24)),
                        [])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseUseInvalidNamespaceTest()
    {
        var (tree, diagnostics) = Parse(
            """
            use Test.1.SubSubNamespace;

            public main(): void { }
            """);

        var expected = new SyntaxTree(
            file,
            null,
            [
                new UseNode(
                    new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(27, 1, 28)),
                    ["Test", "<namespace>", "SubSubNamespace"])
            ],
            [
                new FunctionDeclarationNode(
                    new SourceSpan(new SourcePosition(29, 3, 1), new SourcePosition(52, 3, 24)),
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(
                        new SourceSpan(new SourcePosition(44, 3, 16), new SourcePosition(48, 3, 20)),
                        "void"),
                    new BlockStatementNode(
                        new SourceSpan(new SourcePosition(49, 3, 21), new SourcePosition(52, 3, 24)),
                        [])
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0015ExpectedNamespacePart,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(9, 1, 10), new SourcePosition(10, 1, 11))),
            "Expected a namespace part.");

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseNamespaceAndUseTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test;

            use OtherNamespace;

            public main(): void { }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(15, 1, 16)),
                ["Test"]),
            [
                new UseNode(
                    new SourceSpan(new SourcePosition(17, 3, 1), new SourcePosition(36, 3, 20)),
                    ["OtherNamespace"])
            ],
            [
                new FunctionDeclarationNode(
                    new SourceSpan(new SourcePosition(38, 5, 1), new SourcePosition(61, 5, 24)),
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(
                        new SourceSpan(new SourcePosition(53, 5, 16), new SourcePosition(57, 5, 20)),
                        "void"),
                    new BlockStatementNode(
                        new SourceSpan(new SourcePosition(58, 5, 21), new SourcePosition(61, 5, 24)),
                        [])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }
}