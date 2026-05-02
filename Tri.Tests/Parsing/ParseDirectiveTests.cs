using static Tri.Tests.Helpers;

namespace Tri.Tests.Parsing;

public class ParseDirectiveTests
{
    [Test]
    public void ParseIfDirectiveTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                #if D1

                public type Type1 { }

                #endif
                """));

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                IfDirective
                  DirectiveName: D1
                  Then
                    Type: Type1
                      AccessModifier: public
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseIfDirectiveWithElseTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                #if D1

                public type Type1 { }

                #else

                public type Type2 { }

                #endif
                """));
        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                IfDirective
                  DirectiveName: D1
                  Then
                    Type: Type1
                      AccessModifier: public
                  Else
                    Type: Type2
                      AccessModifier: public
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseNestedIfDirectiveTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                #if D1

                public type Type1 { }

                #if D2

                public type Type2 { }

                #endif

                public type Type3 { }

                #endif
                """));
        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                IfDirective
                  DirectiveName: D1
                  Then
                    Type: Type1
                      AccessModifier: public
                    IfDirective
                      DirectiveName: D2
                      Then
                        Type: Type2
                          AccessModifier: public
                    Type: Type3
                      AccessModifier: public
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseIfDirectiveStatementTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                #if D1
                    print("D1");
                #else
                    print("Empty");
                #endif
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
                      IfDirective
                        DirectiveName: D1
                        Then
                          ExpressionStatement
                            Call
                              MemberAccess
                                Name: print
                              Parameters
                                Literal: String = D1
                        Else
                          ExpressionStatement
                            Call
                              MemberAccess
                                Name: print
                              Parameters
                                Literal: String = Empty
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseEmptyIfDirectiveTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                #if D1
                #else
                #endif
                """));
        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                IfDirective
                  DirectiveName: D1
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }
}