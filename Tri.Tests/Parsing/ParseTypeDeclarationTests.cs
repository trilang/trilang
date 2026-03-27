using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseTypeDeclarationTests
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
    public void ParseTypeTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type Point { }");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseTypeMissingNameTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type { }");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: <>_0
                  AccessModifier: public
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0005ExpectedTypeName,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(30, 3, 13).ToSpan()),
            "Expected a type name.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseTypeMissingOpenBraceTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type Point }");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(36, 3, 19).ToSpan()),
            "Expected '{'.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseTypeMissingCloseBraceTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type Point {");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0014ExpectedTypeMember,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(37, 3, 20).ToSpan()),
            "Expected a type member (a property, a method or a constructor).");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParsePropertiesTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                x: i32;
                y: i32;
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
                  Properties
                    Property: x
                      TypeRef: i32
                    Property: y
                      TypeRef: i32
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParsePropertiesWithBlocksTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                x: i32 {
                    private get {
                        return field;
                    }
                    private set {
                        field = value;
                    }
                }
                y: i32 {
                    private get{
                        return field;
                    }
                    private set {
                        field = value;
                    }
                }
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
                  Properties
                    Property: x
                      TypeRef: i32
                      Getter
                        AccessModifier: private
                        BlockStatement
                          Statements
                            ReturnStatement
                              MemberAccess
                                Name: field
                      Setter
                        AccessModifier: private
                        BlockStatement
                          Statements
                            ExpressionStatement
                              BinaryExpression: Assignment
                                MemberAccess
                                  Name: field
                                MemberAccess
                                  Name: value
                    Property: y
                      TypeRef: i32
                      Getter
                        AccessModifier: private
                        BlockStatement
                          Statements
                            ReturnStatement
                              MemberAccess
                                Name: field
                      Setter
                        AccessModifier: private
                        BlockStatement
                          Statements
                            ExpressionStatement
                              BinaryExpression: Assignment
                                MemberAccess
                                  Name: field
                                MemberAccess
                                  Name: value
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseEmptyGetterTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                x: i32 {
                    private get;
                    private set {
                        field = value;
                    }
                }
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
                  Properties
                    Property: x
                      TypeRef: i32
                      Getter
                        AccessModifier: private
                      Setter
                        AccessModifier: private
                        BlockStatement
                          Statements
                            ExpressionStatement
                              BinaryExpression: Assignment
                                MemberAccess
                                  Name: field
                                MemberAccess
                                  Name: value
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseEmptySetterTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                x: i32 {
                    private get {
                        return field;
                    }
                    private set;
                }
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
                  Properties
                    Property: x
                      TypeRef: i32
                      Getter
                        AccessModifier: private
                        BlockStatement
                          Statements
                            ReturnStatement
                              MemberAccess
                                Name: field
                      Setter
                        AccessModifier: private
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParsePropertyMissingNameTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                : i32;
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0014ExpectedTypeMember,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourceSpan(new SourcePosition(42, 4, 5), new SourcePosition(49, 5, 1))),
            "Expected a type member (a property, a method or a constructor).");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParsePropertyMissingColonTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                x i32;
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
                  Properties
                    Property: x
                      TypeRef: i32
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(44, 4, 7).ToSpan()),
            "Expected ':'.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParsePropertyMissingTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                x: ;
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
                  Properties
                    Property: x
                      FakeType: <>_0
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(45, 4, 8).ToSpan()),
            "Expected a type.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParsePropertyMissingSemiColonTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                x: i32
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
                  Properties
                    Property: x
                      TypeRef: i32
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(49, 5, 1).ToSpan()),
            "Expected ';'.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMethodsTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public toString(): string { }

                public distance(other: Point): f32 { }
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
                  Methods
                    Method: toString
                      AccessModifier: public
                      TypeRef: string
                      BlockStatement
                    Method: distance
                      AccessModifier: public
                      TypeRef: f32
                      BlockStatement
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseMethodMissingNameTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public (): string { }
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
                  Methods
                    Method: <>_0
                      AccessModifier: public
                      TypeRef: string
                      BlockStatement
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0007ExpectedMethodName,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(49, 4, 12).ToSpan()),
            "Expected a method name.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMethodMissingOpenParenTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public toString): string { }
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
                  Methods
                    Method: toString
                      AccessModifier: public
                      TypeRef: string
                      BlockStatement
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(57, 4, 20).ToSpan()),
            "Expected '('.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMethodMissingCloseParenTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public toString(: string { return; }
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
                  Methods
                    Method: toString
                      AccessModifier: public
                      TypeRef: string
                      BlockStatement
                        Statements
                          ReturnStatement
            """;

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(58, 4, 21).ToSpan()),
                "Expected ')'.")
        };

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseMethodMissingColonTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public toString() string { }
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
                  Methods
                    Method: toString
                      AccessModifier: public
                      TypeRef: string
                      BlockStatement
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(60, 4, 23).ToSpan()),
            "Expected ':'.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMethodMissingReturnTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public toString(): { var x: i32 = 1; }
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
                  Methods
                    Method: toString
                      AccessModifier: public
                      Interface
                      BlockStatement
                        Statements
                          Variable: x
                            TypeRef: i32
                            Literal: Integer = 1
            """;

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(63, 4, 26).ToSpan()),
                "Expected '}'."),
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(63, 4, 26).ToSpan()),
                "Expected '{'."),
        };

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseMethodMissingOpenBraceTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public toString(): string }
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
                  Methods
                    Method: toString
                      AccessModifier: public
                      TypeRef: string
                      BlockStatement
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(68, 4, 31).ToSpan()),
            "Expected '{'.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMethodMissingCloseBraceTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public toString(): string {
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
                  Methods
                    Method: toString
                      AccessModifier: public
                      TypeRef: string
                      BlockStatement
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0014ExpectedTypeMember,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(71, 5, 2).ToSpan()),
            "Expected a type member (a property, a method or a constructor).");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMethodMissingCommaTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public toString(a: i32 b: i32): string { }
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
                  Methods
                    Method: toString
                      AccessModifier: public
                      TypeRef: string
                      BlockStatement
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourceSpan(new SourcePosition(65, 4, 28), new SourcePosition(65, 4, 28))),
            "Expected ','.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMethodMissingParameterColonTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public toString(a i32): string { }
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
                  Methods
                    Method: toString
                      AccessModifier: public
                      TypeRef: string
                      BlockStatement
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(60, 4, 23).ToSpan()),
            "Expected ':'.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseMethodMissingParameterTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public toString(a: ): string { }
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
                  Methods
                    Method: toString
                      AccessModifier: public
                      TypeRef: string
                      BlockStatement
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(61, 4, 24).ToSpan()),
            "Expected a type.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseCtorTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point {
                public constructor(x: i32, y: i32) { }
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
                  Constructors
                    Constructor
                      AccessModifier: public
                      BlockStatement
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseTypeWithInterfaceTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type Point : Interface1, Interface2 { }");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
                  Interfaces
                    TypeRef: Interface1
                    TypeRef: Interface2
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseTypeWithMissingInterfaceTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type Point : { }");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0006ExpectedInterface,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(38, 3, 21).ToSpan()),
            "Expected an interface.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseTypeWithMissingCommaInInterfacesTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type Point : Interface1 Interface2 { }");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
                  Interfaces
                    TypeRef: Interface1
                    TypeRef: Interface2
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(49, 3, 32).ToSpan()),
            "Expected ','."
        );

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseTypeWithMissingSecondInterfaceTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type Point : Interface1, { }");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Point
                  AccessModifier: public
                  Interfaces
                    TypeRef: Interface1
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0006ExpectedInterface,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(50, 3, 33).ToSpan()),
            "Expected an interface.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseStaticMethodTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Test {
                public static test(): void { }
            }
            """);
        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Test
                  AccessModifier: public
                  Methods
                    Method: test
                      AccessModifier: public
                      Static: true
                      TypeRef: void
                      BlockStatement
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseCallStaticMethodTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Test {
                public static test(): void { }
            }

            public main(): void {
                Test.test();
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: Test
                  AccessModifier: public
                  Methods
                    Method: test
                      AccessModifier: public
                      Static: true
                      TypeRef: void
                      BlockStatement
                Function: main
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
                    Statements
                      ExpressionStatement
                        Call
                          MemberAccess
                            MemberAccess
                              Name: Test
                            Name: test
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }
}