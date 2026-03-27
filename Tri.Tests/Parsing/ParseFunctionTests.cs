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

       const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Function: test
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(29, 3, 12).ToSpan()),
            "Expected '('.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMissingParamColonFunctionTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test(x i32): void { }");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Function: test
                  AccessModifier: public
                  Parameters
                    Parameter: x
                      TypeRef: i32
                  TypeRef: void
                  BlockStatement
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(32, 3, 15).ToSpan()),
            "Expected ':'.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMissingParamTypeFunctionTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test(x: ): void { }");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Function: test
                  AccessModifier: public
                  Parameters
                    Parameter: x
                      FakeType: <>_0
                  TypeRef: void
                  BlockStatement
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(33, 3, 16).ToSpan()),
            "Expected a type.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMissingCommaFunctionTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test(x: i32 y: i32): void { }");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Function: test
                  AccessModifier: public
                  Parameters
                    Parameter: x
                      TypeRef: i32
                    Parameter: y
                      TypeRef: i32
                  TypeRef: void
                  BlockStatement
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(37, 3, 20).ToSpan()),
            "Expected ','."
        );

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMissingCloseParenFunctionTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test( : void { return; }");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Function: test
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
                    Statements
                      ReturnStatement
            """;

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(31, 3, 14).ToSpan()),
                "Expected ')'."),
        };

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseMissingReturnColonFunctionTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test() void { return; }");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Function: test
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
                    Statements
                      ReturnStatement
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(32, 3, 15).ToSpan()),
            "Expected ':'."
        );

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMissingReturnTypeFunctionTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test(): { return; }");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Function: test
                  AccessModifier: public
                  Interface
                  BlockStatement
                    Statements
                      ReturnStatement
            """;

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

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseMissingBlockInFunctionTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test(): void");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Function: test
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
            """;

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

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseEmptyFunctionTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test(): void { }");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Function: test
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseSingleParameterFunctionTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test(x: i32): void { }");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Function: test
                  AccessModifier: public
                  Parameters
                    Parameter: x
                      TypeRef: i32
                  TypeRef: void
                  BlockStatement
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseMultipleParametersFunctionTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test(x: i32, y: i32, z: i32): void { }");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Function: test
                  AccessModifier: public
                  Parameters
                    Parameter: x
                      TypeRef: i32
                    Parameter: y
                      TypeRef: i32
                    Parameter: z
                      TypeRef: i32
                  TypeRef: void
                  BlockStatement
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseArrayTypeTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test(x: i32[]): void { }");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Function: test
                  AccessModifier: public
                  Parameters
                    Parameter: x
                      ArrayType
                        TypeRef: i32
                  TypeRef: void
                  BlockStatement
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }
}