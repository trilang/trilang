using static Tri.Tests.Helpers;

namespace Tri.Tests.Parsing;

public class ParseCallExpressionTests
{
    [Test]
    public void ParseCallStatementTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    print("Hello, World!");
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
                      ExpressionStatement
                        Call
                          MemberAccess
                            Name: print
                          Parameters
                            Literal: String = Hello, World!
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseCallStatementMultipleParamsTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    sum(1, 2, 3);
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
                      ExpressionStatement
                        Call
                          MemberAccess
                            Name: sum
                          Parameters
                            Literal: Integer = 1
                            Literal: Integer = 2
                            Literal: Integer = 3
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseCallExpressionMultipleParamsTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    var x: i32 = 1 + sum(1, 2, 3);
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
                      Variable: x
                        TypeRef: i32
                        BinaryExpression: Addition
                          Literal: Integer = 1
                          Call
                            MemberAccess
                              Name: sum
                            Parameters
                              Literal: Integer = 1
                              Literal: Integer = 2
                              Literal: Integer = 3
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }
}