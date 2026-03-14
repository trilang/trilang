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
                default,
                ["Test"]),
            [],
            [
                new FunctionDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(
                        default,
                        "void"),
                    new BlockStatementNode(
                        default,
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
                default,
                ["Test", "SubNamespace", "SubSubNamespace"]),
            [],
            [
                new FunctionDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(
                        default,
                        "void"),
                    new BlockStatementNode(
                        default,
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
            namespace Test1;

            use Test;

            public main(): void { }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [
                new UseNode(
                    default,
                    ["Test"])
            ],
            [
                new FunctionDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(
                        default,
                        "void"),
                    new BlockStatementNode(
                        default,
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
            namespace Test1;

            use Test.SubNamespace.SubSubNamespace;

            public main(): void { }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [
                new UseNode(
                    default,
                    ["Test", "SubNamespace", "SubSubNamespace"])
            ],
            [
                new FunctionDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(
                        default,
                        "void"),
                    new BlockStatementNode(
                        default,
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
            namespace Test1;

            use Test.1.SubSubNamespace;

            public main(): void { }
            """);

        var expected = new SyntaxTree(
            file,
            new NamespaceNode(default, ["Test1"]),
            [
                new UseNode(
                    default,
                    ["Test", "<namespace>", "SubSubNamespace"])
            ],
            [
                new FunctionDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(
                        default,
                        "void"),
                    new BlockStatementNode(
                        default,
                        [])
                )
            ]);

        var diagnostic = new Diagnostic(
            DiagnosticId.P0015ExpectedNamespacePart,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(27, 3, 10), new SourcePosition(28, 3, 11))),
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
                default,
                ["Test"]),
            [
                new UseNode(
                    default,
                    ["OtherNamespace"])
            ],
            [
                new FunctionDeclarationNode(
                    default,
                    AccessModifier.Public,
                    "main",
                    [],
                    new TypeRefNode(
                        default,
                        "void"),
                    new BlockStatementNode(
                        default,
                        [])
                )
            ]);

        Assert.That(tree, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }
}