using Trilang;
using Trilang.Compilation.Diagnostics;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Parsing;

public class ParseLoopTests
{
    [Test]
    public void ParseWhileTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(x: i32): void {
                while (x > 0) {
                }
            }
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
        var file = CreateFile(
            """
            namespace Test1;

            public test(x: i32): void {
                while x > 0) {
                }
            }
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
        var file = CreateFile(
            """
            namespace Test1;

            public test(x: i32): void {
                while (;) {
                }
            }
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
        var file = CreateFile(
            """
            namespace Test1;

            public test(x: i32): void {
                while (x > 0 {
                }
            }
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
        var file = CreateFile(
            """
            namespace Test1;

            public test(x: i32): void {
                while (x > 0)
                   ;
            }
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
        var file = CreateFile(
            """
            namespace Test1;

            public test(x: i32): void {
                while (x > 0) {
                    break;
                }
            }
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
        var file = CreateFile(
            """
            namespace Test1;

            public test(x: i32): void {
                while (x > 0) {
                    continue;
                }
            }
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