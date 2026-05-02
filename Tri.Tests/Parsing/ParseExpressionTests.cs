using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Parsing.Ast;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Parsing;

public class ParseExpressionTests
{
    [Test]
    public void ParseVariableTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    var x: i32 = 5;
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
                        Literal: Integer = 5
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseUnaryPlusTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    var x: i32 = +2;
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
                        UnaryExpression: UnaryPlus
                          Literal: Integer = 2
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseUnaryMinusTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    var x: i32 = -2;
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
                        UnaryExpression: UnaryMinus
                          Literal: Integer = 2
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseLogicalNotTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    var x: i32 = !2;
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
                        UnaryExpression: LogicalNot
                          Literal: Integer = 2
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseBitwiseNotTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    var x: i32 = ~2;
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
                        UnaryExpression: BitwiseNot
                          Literal: Integer = 2
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void MultipleUnaryOperatorsTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    var x: i32 = ~+2;
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
                        UnaryExpression: BitwiseNot
                          UnaryExpression: UnaryPlus
                            Literal: Integer = 2
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    [TestCase("+", BinaryExpressionKind.Addition)]
    [TestCase("-", BinaryExpressionKind.Subtraction)]
    [TestCase("*", BinaryExpressionKind.Multiplication)]
    [TestCase("/", BinaryExpressionKind.Division)]
    [TestCase("%", BinaryExpressionKind.Modulus)]
    public void ParseBinaryNumberTest(string @operator, BinaryExpressionKind kind)
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                $$"""
                  namespace Test1;

                  public main(): void {
                      var x: i32 = 2 {{@operator}} 2;
                  }
                  """));
        var expected =
            $"""
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
                         BinaryExpression: {kind}
                           Literal: Integer = 2
                           Literal: Integer = 2
             """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseBitwiseAndTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    var x: i32 = 2 & 2;
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
                        BinaryExpression: BitwiseAnd
                          Literal: Integer = 2
                          Literal: Integer = 2
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseBitwiseOrTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    var x: i32 = 2 | 2;
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
                        BinaryExpression: BitwiseOr
                          Literal: Integer = 2
                          Literal: Integer = 2
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseBitwiseXorTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    var x: i32 = 2 ^ 2;
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
                        BinaryExpression: BitwiseXor
                          Literal: Integer = 2
                          Literal: Integer = 2
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseLogicalAndTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    var x: bool = true && true;
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
                        TypeRef: bool
                        BinaryExpression: ConditionalAnd
                          Literal: Boolean = True
                          Literal: Boolean = True
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseLogicalOrTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    var x: bool = true || true;
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
                        TypeRef: bool
                        BinaryExpression: ConditionalOr
                          Literal: Boolean = True
                          Literal: Boolean = True
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseEqualityTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    var x: bool = true == true;
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
                        TypeRef: bool
                        BinaryExpression: Equality
                          Literal: Boolean = True
                          Literal: Boolean = True
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseInequalityTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    var x: bool = true != true;
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
                        TypeRef: bool
                        BinaryExpression: Inequality
                          Literal: Boolean = True
                          Literal: Boolean = True
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseLessThanTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    var x: bool = 2 < 2;
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
                        TypeRef: bool
                        BinaryExpression: LessThan
                          Literal: Integer = 2
                          Literal: Integer = 2
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseLessThanOrEqualTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    var x: bool = 2 <= 2;
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
                        TypeRef: bool
                        BinaryExpression: LessThanOrEqual
                          Literal: Integer = 2
                          Literal: Integer = 2
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseGreaterThanTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    var x: bool = 2 > 2;
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
                        TypeRef: bool
                        BinaryExpression: GreaterThan
                          Literal: Integer = 2
                          Literal: Integer = 2
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseGreaterThanOrEqualTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    var x: bool = 2 >= 2;
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
                        TypeRef: bool
                        BinaryExpression: GreaterThanOrEqual
                          Literal: Integer = 2
                          Literal: Integer = 2
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseAssignmentTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    x = 1;
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
                        BinaryExpression: Assignment
                          MemberAccess
                            Name: x
                          Literal: Integer = 1
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    [TestCase("+=", BinaryExpressionKind.AdditionAssignment)]
    [TestCase("-=", BinaryExpressionKind.SubtractionAssignment)]
    [TestCase("*=", BinaryExpressionKind.MultiplicationAssignment)]
    [TestCase("/=", BinaryExpressionKind.DivisionAssignment)]
    [TestCase("%=", BinaryExpressionKind.ModulusAssignment)]
    [TestCase("&=", BinaryExpressionKind.BitwiseAndAssignment)]
    [TestCase("|=", BinaryExpressionKind.BitwiseOrAssignment)]
    [TestCase("^=", BinaryExpressionKind.BitwiseXorAssignment)]
    public void ParseAssignment2Test(string @operator, BinaryExpressionKind kind)
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                $$"""
                  namespace Test1;

                  public main(): void {
                      x {{@operator}} 1;
                  }
                  """));

        var expected =
            $$"""
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
                          BinaryExpression: {{kind}}
                            MemberAccess
                              Name: x
                            Literal: Integer = 1
              """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseParenTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    var x: i32 = (1 + 2) * 3;
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
                        BinaryExpression: Multiplication
                          BinaryExpression: Addition
                            Literal: Integer = 1
                            Literal: Integer = 2
                          Literal: Integer = 3
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseVariableExpressionTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(y: i32): void {
                    var x: i32 = 2 * y;
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
                  Parameters
                    Parameter: y
                      TypeRef: i32
                  TypeRef: void
                  BlockStatement
                    Statements
                      Variable: x
                        TypeRef: i32
                        BinaryExpression: Multiplication
                          Literal: Integer = 2
                          MemberAccess
                            Name: y
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParsePrecedenceTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    var x: i32 = true || true && false | false ^ false & true == 1 + 2 * 3 < 10;
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
                        BinaryExpression: ConditionalOr
                          Literal: Boolean = True
                          BinaryExpression: ConditionalAnd
                            Literal: Boolean = True
                            BinaryExpression: BitwiseOr
                              Literal: Boolean = False
                              BinaryExpression: BitwiseXor
                                Literal: Boolean = False
                                BinaryExpression: BitwiseAnd
                                  Literal: Boolean = False
                                  BinaryExpression: Equality
                                    Literal: Boolean = True
                                    BinaryExpression: LessThan
                                      BinaryExpression: Addition
                                        Literal: Integer = 1
                                        BinaryExpression: Multiplication
                                          Literal: Integer = 2
                                          Literal: Integer = 3
                                      Literal: Integer = 10
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void TupleExpressionTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    return (1, 2);
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
                      ReturnStatement
                        TupleExpression
                          Expressions
                            Literal: Integer = 1
                            Literal: Integer = 2
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void TupleExpressionMissingCloseParenTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): void {
                return (1, 2;
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
                      ReturnStatement
                        TupleExpression
                          Expressions
                            Literal: Integer = 1
                            Literal: Integer = 2
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(56, 4, 17).ToSpan()),
            "Expected ')'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseNewArrayTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public main(): void {
                    return new i32[10];
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
                      ReturnStatement
                        NewArray
                          ArrayType
                            TypeRef: i32
                          Literal: Integer = 10
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseAsExpressionTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public test(a: i32): i8 {
                    return a is i8;
                }
                """));

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Function: test
                  AccessModifier: public
                  Parameters
                    Parameter: a
                      TypeRef: i32
                  TypeRef: i8
                  BlockStatement
                    Statements
                      ReturnStatement
                        IsExpression
                          MemberAccess
                            Name: a
                          TypeRef: i8
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseAsExpressionMissingTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(a: i32): i8 {
                return a is;
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
                    Parameter: a
                      TypeRef: i32
                  TypeRef: i8
                  BlockStatement
                    Statements
                      ReturnStatement
                        IsExpression
                          MemberAccess
                            Name: a
                          FakeType: <>_0
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(59, 4, 16).ToSpan()),
            "Expected a type.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseCastExpressionTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public test(a: i32): i8 {
                    return (i8)a;
                }
                """));

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Function: test
                  AccessModifier: public
                  Parameters
                    Parameter: a
                      TypeRef: i32
                  TypeRef: i8
                  BlockStatement
                    Statements
                      ReturnStatement
                        Cast
                          TypeRef: i8
                          MemberAccess
                            Name: a
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseCastExpressionMissingCloseParenTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(a: i32): i8 {
                return (i8 a;
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
                    Parameter: a
                      TypeRef: i32
                  TypeRef: i8
                  BlockStatement
                    Statements
                      ReturnStatement
                        Cast
                          TypeRef: i8
                          MemberAccess
                            Name: a
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(59, 4, 16).ToSpan()),
            "Expected ')'."
        );

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseCastExpressionMissingExpressionTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(a: i32): i8 {
                return (i8);
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
                    Parameter: a
                      TypeRef: i32
                  TypeRef: i8
                  BlockStatement
                    Statements
                      ReturnStatement
                        Cast
                          TypeRef: i8
                          FakeExpression
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        var diagnostic = new Diagnostic(
            DiagnosticId.P0009ExpectedExpression,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(59, 4, 16).ToSpan()),
            "Expected an expression."
        );

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseFloatingNumberTest()
    {
        var (tree, diagnostics) = ParseFile(
            CreateFile(
                """
                namespace Test1;

                public test(): f64 {
                    return 3.14;
                }
                """));

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Function: test
                  AccessModifier: public
                  TypeRef: f64
                  BlockStatement
                    Statements
                      ReturnStatement
                        Literal: Float = 3.14
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }
}