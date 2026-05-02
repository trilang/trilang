using Trilang;
using Trilang.Compilation.Diagnostics;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Parsing;

public class ParseFunctionTests
{
    [Test]
    public void ParseMissingOpenParenFunctionTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test): void { }
            """);
        var (tree, diagnostics) = ParseFile(file);

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
        var file = CreateFile(
            """
            namespace Test1;

            public test(x i32): void { }
            """);
        var (tree, diagnostics) = ParseFile(file);

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
        var file = CreateFile(
            """
            namespace Test1;

            public test(x: ): void { }
            """);
        var (tree, diagnostics) = ParseFile(file);

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
        var file = CreateFile(
            """
            namespace Test1;

            public test(x: i32 y: i32): void { }
            """);
        var (tree, diagnostics) = ParseFile(file);

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
        var file = CreateFile(
            """
            namespace Test1;

            public test( : void { return; }
            """);
        var (tree, diagnostics) = ParseFile(file);

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
        var file = CreateFile(
            """
            namespace Test1;

            public test() void { return; }
            """);
        var (tree, diagnostics) = ParseFile(file);

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
        var file = CreateFile(
            """
            namespace Test1;

            public test(): { return; }
            """);
        var (tree, diagnostics) = ParseFile(file);

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
        var file = CreateFile(
            """
            namespace Test1;

            public test(): void
            """);
        var (tree, diagnostics) = ParseFile(file);

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
        var file = CreateFile(
            """
            namespace Test1;

            public test(): void { }
            """);
        var (tree, diagnostics) = ParseFile(file);

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
        var file = CreateFile(
            """
            namespace Test1;

            public test(x: i32): void { }
            """);
        var (tree, diagnostics) = ParseFile(file);

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
        var file = CreateFile(
            """
            namespace Test1;

            public test(x: i32, y: i32, z: i32): void { }
            """);
        var (tree, diagnostics) = ParseFile(file);

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
        var file = CreateFile(
            """
            namespace Test1;

            public test(x: i32[]): void { }
            """);
        var (tree, diagnostics) = ParseFile(file);

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