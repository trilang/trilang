using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseMemberAccessTests
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
    public void ParseArrayAccessTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(x: i32[]): void {
                var a: i32 = x[0];
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
                      ArrayType
                        TypeRef: i32
                  TypeRef: void
                  BlockStatement
                    Statements
                      Variable: a
                        TypeRef: i32
                        ArrayAccess
                          MemberAccess
                            Name: x
                          Literal: Integer = 0
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseArrayAccessMissingCloseTest()
    {
        const string code =
            """
            namespace Test1;

            public test(x: i32[]): void {
                var a: i32 = x[0;
            }
            """;
        var (tree, diagnostics) = Parse(code);

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
                    Statements
                      Variable: a
                        TypeRef: i32
                        ArrayAccess
                          MemberAccess
                            Name: x
                          Literal: Integer = 0
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(68, 4, 21).ToSpan()),
            "Expected ']'.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseSetArrayTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(): void {
                x[0] = 1;
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
                  TypeRef: void
                  BlockStatement
                    Statements
                      ExpressionStatement
                        BinaryExpression: Assignment
                          ArrayAccess
                            MemberAccess
                              Name: x
                            Literal: Integer = 0
                          Literal: Integer = 1
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseSetNestedArrayTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(): void {
                a.b[0].c = 1;
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
                  TypeRef: void
                  BlockStatement
                    Statements
                      ExpressionStatement
                        BinaryExpression: Assignment
                          MemberAccess
                            ArrayAccess
                              MemberAccess
                                MemberAccess
                                  Name: a
                                Name: b
                              Literal: Integer = 0
                            Name: c
                          Literal: Integer = 1
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseMultipleArrayAccessTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                return a[0][1];
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
                      ReturnStatement
                        ArrayAccess
                          ArrayAccess
                            MemberAccess
                              Name: a
                            Literal: Integer = 0
                          Literal: Integer = 1
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseMultipleMemberAccessTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                return a.b.c;
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
                      ReturnStatement
                        MemberAccess
                          MemberAccess
                            MemberAccess
                              Name: a
                            Name: b
                          Name: c
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseMultipleMemberAccessMissingExpressionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                return a.b.;
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
                      ReturnStatement
                        FakeExpression
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0013ExpectedIdentifier,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourceSpan(new SourcePosition(55, 4, 16), new SourcePosition(56, 4, 17))),
            "Expected an identifier.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMemberAccessNestedCallTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                return a.b().c;
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
                      ReturnStatement
                        MemberAccess
                          Call
                            MemberAccess
                              MemberAccess
                                Name: a
                              Name: b
                          Name: c
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseMultipleCallsTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                f(1)(2);
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
                      ExpressionStatement
                        Call
                          Call
                            MemberAccess
                              Name: f
                            Parameters
                              Literal: Integer = 1
                          Parameters
                            Literal: Integer = 2
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseMemberAccessAfterCtorTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                return new Test().a;
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
                      ReturnStatement
                        MemberAccess
                          NewObject
                            TypeRef: Test
                          Name: a
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseMemberAccessAfterNewArrayTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                return new i32[0].size;
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
                      ReturnStatement
                        MemberAccess
                          NewArray
                            ArrayType
                              TypeRef: i32
                            Literal: Integer = 0
                          Name: size
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseCallExpWithBinaryExpTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                return 1.toString();
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
                      ReturnStatement
                        Call
                          MemberAccess
                            Literal: Integer = 1
                            Name: toString
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseCallExpWithParenExpTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                return 1 + 2.toString();
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
                      ReturnStatement
                        BinaryExpression: Addition
                          Literal: Integer = 1
                          Call
                            MemberAccess
                              Literal: Integer = 2
                              Name: toString
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseTupleMemberAccessTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(t: (i32, string)): i32 {
                return t.0;
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
                    Parameter: t
                      TupleType
                        Types
                          TypeRef: i32
                          TypeRef: string
                  TypeRef: i32
                  BlockStatement
                    Statements
                      ReturnStatement
                        MemberAccess
                          MemberAccess
                            Name: t
                          Name: 0
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseTupleMemberAccessWithIncorrectIndexTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(t: (i32, string)): i32 {
                return t.0x;
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
                    Parameter: t
                      TupleType
                        Types
                          TypeRef: i32
                          TypeRef: string
                  TypeRef: i32
                  BlockStatement
                    Statements
                      ReturnStatement
                        MemberAccess
                          MemberAccess
                            Name: t
                          Name: 0
                      ExpressionStatement
                        MemberAccess
                          Name: x
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(69, 4, 15).ToSpan()),
            "Expected ';'.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }
}