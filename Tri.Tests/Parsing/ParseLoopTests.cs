using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseLoopTests
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
    public void ParseWhileTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(x: i32): void {
                while (x > 0) {
                }
            }
            """);

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
                    Statements
                      While
                        BinaryExpression: GreaterThan
                          MemberAccess
                            Name: x
                          Literal: Integer = 0
                        BlockStatement
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseWhileMissingOpenParenTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(x: i32): void {
                while x > 0) {
                }
            }
            """);

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
                    Statements
                      While
                        BinaryExpression: GreaterThan
                          MemberAccess
                            Name: x
                          Literal: Integer = 0
                        BlockStatement
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(56, 4, 11).ToSpan()),
            "Expected '('.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseWhileMissingConditionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(x: i32): void {
                while (;) {
                }
            }
            """);

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
                    Statements
                      While
                        FakeExpression
                        BlockStatement
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0009ExpectedExpression,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourceSpan(new SourcePosition(57, 4, 12), new SourcePosition(58, 4, 13))),
            "Expected an expression.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseWhileMissingCloseParenTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(x: i32): void {
                while (x > 0 {
                }
            }
            """);

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
                    Statements
                      While
                        BinaryExpression: GreaterThan
                          MemberAccess
                            Name: x
                          Literal: Integer = 0
                        BlockStatement
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(63, 4, 18).ToSpan()),
            "Expected ')'.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseWhileMissingBlockTest()
    {
        var source = """
                     namespace Test1;

                     public test(x: i32): void {
                         while (x > 0)
                            ;
                     }
                     """;

        var (tree, diagnostics) = Parse(source);

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
                    Statements
                      While
                        BinaryExpression: GreaterThan
                          MemberAccess
                            Name: x
                          Literal: Integer = 0
                        BlockStatement
                          Statements
                            FakeStatement
            """;

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(71, 5, 8).ToSpan()),
                "Expected '{'."),
            new Diagnostic(
                DiagnosticId.P0004ExpectedStatement,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(71, 5, 8).ToSpan()),
                "Expected a statement."),
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(74, 6, 2).ToSpan()),
                "Expected '}'."),
        };

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseBreakTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(x: i32): void {
                while (x > 0) {
                    break;
                }
            }
            """);

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
                    Statements
                      While
                        BinaryExpression: GreaterThan
                          MemberAccess
                            Name: x
                          Literal: Integer = 0
                        BlockStatement
                          Statements
                            Break
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseContinueTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(x: i32): void {
                while (x > 0) {
                    continue;
                }
            }
            """);

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
                    Statements
                      While
                        BinaryExpression: GreaterThan
                          MemberAccess
                            Name: x
                          Literal: Integer = 0
                        BlockStatement
                          Statements
                            Continue
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }
}