using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Parsing.Ast;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Parsing;

public class ParseNumberTests
{
    [Test]
    [TestCase("42", null, LiteralExpressionKind.I32)]
    [TestCase("42", "i8", LiteralExpressionKind.I8)]
    [TestCase("42", "i16", LiteralExpressionKind.I16)]
    [TestCase("42", "i32", LiteralExpressionKind.I32)]
    [TestCase("42", "i64", LiteralExpressionKind.I64)]
    [TestCase("42", "u8", LiteralExpressionKind.U8)]
    [TestCase("42", "u16", LiteralExpressionKind.U16)]
    [TestCase("42", "u32", LiteralExpressionKind.U32)]
    [TestCase("42", "u64", LiteralExpressionKind.U64)]
    [TestCase("3.14", null, LiteralExpressionKind.F64)]
    [TestCase("3.14", "f32", LiteralExpressionKind.F32)]
    [TestCase("3.14", "f64", LiteralExpressionKind.F64)]
    public void ParseNumberTest(string number, string? suffix, LiteralExpressionKind kind)
    {
        var file = CreateFile(
            $$"""
              namespace Test1;

              public test(): void {
                  return {{number}}{{suffix}};
              }
              """);

        var (tree, diagnostics) = ParseFile(file);

        var expected =
            $"""
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
                         Literal: {kind} = {number}
             """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void ParseInvalidSuffix()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(): void {
                return 42x32;
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
                  TypeRef: void
                  BlockStatement
                    Statements
                      ReturnStatement
                        Literal: I32 = 42
                      ExpressionStatement
                        MemberAccess
                          Name: x32
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(53, 4, 14), new SourcePosition(53, 4, 14))),
            "Expected ';'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void ParseBigNumberForTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(): void {
                var x: i8 = 256i8;
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
                  TypeRef: void
                  BlockStatement
                    Statements
                      Variable: x
                        TypeRef: i8
                        Literal: I8 = 0
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0017InvalidNumber,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(56, 4, 17), new SourcePosition(61, 4, 22))),
            "Invalid number.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
    }
}