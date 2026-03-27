using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseNewOperatorTests
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
    public void ParseNewOperatorTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                var p: Point = new Point();
            }
            """);

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
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                var p: Point = new Point(1, 2);
            }
            """);

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
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                var p: Point = new ();
            }
            """);

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
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                var p: Point = new Point(1, );
            }
            """);

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
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                var p: Point = new Point(;
            }
            """);

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