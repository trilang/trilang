using Trilang;
using Trilang.Compilation.Diagnostics;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Parsing;

public class ParseNewOperatorTests
{
    [Test]
    public void ParseNewOperatorTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    var p: Point = new Point();
                }
                """));

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Function: main
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
                    Statements
                      Variable: p
                        TypeRef: Point
                        NewObject
                          TypeRef: Point
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseNewOperatorWithParametersTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): void {
                var p: Point = new Point(1, 2);
            }
            """);
        var (tree, diagnostics) = ParseFile(file);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Function: main
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
                    Statements
                      Variable: p
                        TypeRef: Point
                        NewObject
                          TypeRef: Point
                          Parameters
                            Literal: Integer = 1
                            Literal: Integer = 2
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseNewOperatorMissingTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): void {
                var p: Point = new ();
            }
            """);
        var (tree, diagnostics) = ParseFile(file);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Function: main
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
                    Statements
                      Variable: p
                        TypeRef: Point
                        NewObject
                          FakeType: <>_0
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(63, 4, 24).ToSpan()),
            "Expected a type.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseNewOperatorMissingArgumentTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): void {
                var p: Point = new Point(1, );
            }
            """);
        var (tree, diagnostics) = ParseFile(file);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Function: main
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
                    Statements
                      Variable: p
                        TypeRef: Point
                        NewObject
                          TypeRef: Point
                          Parameters
                            Literal: Integer = 1
                            FakeExpression
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0009ExpectedExpression,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(72, 4, 33).ToSpan()),
            "Expected an expression.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseNewOperatorMissingCloseParenTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): void {
                var p: Point = new Point(;
            }
            """);
        var (tree, diagnostics) = ParseFile(file);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Function: main
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
                    Statements
                      Variable: p
                        TypeRef: Point
                        NewObject
                          TypeRef: Point
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(69, 4, 30).ToSpan()),
            "Expected ')'.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }
}