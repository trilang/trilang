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
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type List<T> { }");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: List
                  AccessModifier: public
                  Generic Arguments
                    TypeRef: T
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseGenericTypeMissingTypeTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type List<> { }");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: List
                  AccessModifier: public
                  Generic Arguments
                    FakeType: <>_0
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(35, 3, 18).ToSpan()),
            "Expected a type.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseGenericTypeMissingSecondTypeTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type List<T,> { }");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: List
                  AccessModifier: public
                  Generic Arguments
                    TypeRef: T
                    FakeType: <>_0
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(37, 3, 20).ToSpan()),
            "Expected a type.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseGenericTypeMissingGreaterTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type List<T { }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: List
                  AccessModifier: public
                  Generic Arguments
                    TypeRef: T
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(37, 3, 20).ToSpan()),
            "Expected '>'.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseAliasToGenericTypeTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T = List<i32, bool>;");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  GenericApplication
                    TypeRef: List
                    TypeArguments
                      TypeRef: i32
                      TypeRef: bool
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseNestedGenericTypeAliasTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T = List<i32, List<bool>>;");
        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  GenericApplication
                    TypeRef: List
                    TypeArguments
                      TypeRef: i32
                      GenericApplication
                        TypeRef: List
                        TypeArguments
                          TypeRef: bool
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseAliasToGenericTypeMissingTypeTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T = List<>;");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  GenericApplication
                    TypeRef: List
                    TypeArguments
                      FakeType: <>_0
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(39, 3, 22).ToSpan()),
            "Expected a type.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseAliasToGenericTypeMissingSecondTypeTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T = List<i32, >;");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  GenericApplication
                    TypeRef: List
                    TypeArguments
                      TypeRef: i32
                      FakeType: <>_0
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(44, 3, 27).ToSpan()),
            "Expected a type.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseAliasToGenericTypeMissingCloseAngleBracketTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T = List<i32;");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  GenericApplication
                    TypeRef: List
                    TypeArguments
                      TypeRef: i32
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(42, 3, 25).ToSpan()),
            "Expected '>'.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseGenericTypeAliasTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T<T1, T2> = T1 | T2;");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  DiscriminatedUnion
                    Types
                      TypeRef: T1
                      TypeRef: T2
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseGenericTypeAliasMissingTypeTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T<> = T1 | T2;");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  DiscriminatedUnion
                    Types
                      TypeRef: T1
                      TypeRef: T2
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(32, 3, 15).ToSpan()),
            "Expected a type.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseGenericTypeAliasMissingSecondTypeTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T<T1, > = T1 | T2;");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  DiscriminatedUnion
                    Types
                      TypeRef: T1
                      TypeRef: T2
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(36, 3, 19).ToSpan()),
            "Expected a type.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseGenericTypeAliasMissingCloseAngleBracketTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T<T1, T2 = T1 | T2;");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  DiscriminatedUnion
                    Types
                      TypeRef: T1
                      TypeRef: T2
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(39, 3, 22).ToSpan()),
            "Expected '>'.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }
}