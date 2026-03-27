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

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test
              Declarations
                Function: main
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
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

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test.SubNamespace.SubSubNamespace
              Declarations
                Function: main
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
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

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              UseNodes
                Use
                  Parts: Test
              Declarations
                Function: main
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
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

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              UseNodes
                Use
                  Parts: Test.SubNamespace.SubSubNamespace
              Declarations
                Function: main
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
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

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              UseNodes
                Use
                  Parts: Test.<namespace>.SubSubNamespace
              Declarations
                Function: main
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0015ExpectedNamespacePart,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(27, 3, 10), new SourcePosition(28, 3, 11))),
            "Expected a namespace part.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
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

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test
              UseNodes
                Use
                  Parts: OtherNamespace
              Declarations
                Function: main
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseFullyQualifiedNameTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type T = Some.NS.TestType;
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  TypeRef: Some.NS.TestType
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }
}