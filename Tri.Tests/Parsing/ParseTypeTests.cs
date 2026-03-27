using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseTypeTests
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
    public void ParseTypeAliasTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type MyType = i32;");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: MyType
                  AccessModifier: public
                  TypeRef: i32
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseTypeAliasMissingNameTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type = i32;");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: <>_0
                  AccessModifier: public
                  TypeRef: i32
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
    public void ParseTypeAliasMissingTypeTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type MyType = ;");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: MyType
                  AccessModifier: public
                  FakeType: <>_0
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(39, 3, 22).ToSpan()),
            "Expected a type.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseTypeAliasMissingSemiColonTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type MyType = i32");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: MyType
                  AccessModifier: public
                  TypeRef: i32
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(42, 3, 25).ToSpan()),
            "Expected ';'.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseFunctionTypeTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type F = () => void;");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: F
                  AccessModifier: public
                  FunctionType
                    TypeRef: void
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseFunctionTypeWithParametersTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type F = (i32, i32) => i32;");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: F
                  AccessModifier: public
                  FunctionType
                    Parameters
                      TypeRef: i32
                      TypeRef: i32
                    TypeRef: i32
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseFunctionTypeMissingNameTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type = (i32, i32) => i32;");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: <>_0
                  AccessModifier: public
                  FunctionType
                    Parameters
                      TypeRef: i32
                      TypeRef: i32
                    TypeRef: i32
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
    public void ParseFunctionTypeMissingEqualTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type F (i32, i32) => i32;");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: F
                  AccessModifier: public
            """;

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(32, 3, 15).ToSpan()),
                "Expected '{'."),
            new Diagnostic(
                DiagnosticId.P0014ExpectedTypeMember,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourceSpan(new SourcePosition(32, 3, 15), new SourcePosition(50, 3, 33))),
                "Expected a type member (a property, a method or a constructor).")
        };

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseFunctionTypeMissingOpenParenTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type F = i32, i32) => i32;");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: F
                  AccessModifier: public
                  TypeRef: i32
                FakeDeclaration
            """;

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(37, 3, 20).ToSpan()),
                "Expected ';'."),
            new Diagnostic(
                DiagnosticId.P0010ExpectedDeclaration,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourceSpan(new SourcePosition(37, 3, 20), new SourcePosition(51, 3, 34))),
                "Expected a type or a function.")
        };

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseFunctionTypeMissingCloseParenTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type F = (i32, i32 => i32;");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: F
                  AccessModifier: public
                  FunctionType
                    Parameters
                      TypeRef: i32
                      TypeRef: i32
                    TypeRef: i32
            """;

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(44, 3, 27).ToSpan()),
                "Expected ')'.")
        };

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseFunctionTypeCommaTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type F = (i32 i32) => i32;");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: F
                  AccessModifier: public
                  FunctionType
                    Parameters
                      TypeRef: i32
                      TypeRef: i32
                    TypeRef: i32
            """;

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(39, 3, 22).ToSpan()),
                "Expected ','.")
        };

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseFunctionTypeArrowTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type F = (i32, i32) i32;");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: F
                  AccessModifier: public
                  TupleType
                    Types
                      TypeRef: i32
                      TypeRef: i32
                FakeDeclaration
            """;

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(45, 3, 28).ToSpan()),
                "Expected ';'."),
            new Diagnostic(
                DiagnosticId.P0010ExpectedDeclaration,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourceSpan(new SourcePosition(45, 3, 28), new SourcePosition(49, 3, 32))),
                "Expected a type or a function.")
        };

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseFunctionTypeReturnTypeTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type F = (i32, i32) => ;");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: F
                  AccessModifier: public
                  FunctionType
                    Parameters
                      TypeRef: i32
                      TypeRef: i32
                    FakeType: <>_0
            """;

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0003ExpectedType,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(48, 3, 31).ToSpan()),
                "Expected a type.")
        };

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseFunctionTypeSemiColonTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type F = (i32, i32) => i32");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: F
                  AccessModifier: public
                  FunctionType
                    Parameters
                      TypeRef: i32
                      TypeRef: i32
                    TypeRef: i32
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(51, 3, 34).ToSpan()),
            "Expected ';'.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseArrayOfFunctionTypesTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type T = (() => i32)[];
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  ArrayType
                    FunctionType
                      TypeRef: i32
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void ParseFunctionTypeInParameterTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test(callback: (i32, i32) => void): void { }");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Function: test
                  AccessModifier: public
                  Parameters
                    Parameter: callback
                      FunctionType
                        Parameters
                          TypeRef: i32
                          TypeRef: i32
                        TypeRef: void
                  TypeRef: void
                  BlockStatement
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseFunctionTypeInReturnTypeTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic test(): (i32, i32) => void { }");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Function: test
                  AccessModifier: public
                  FunctionType
                    Parameters
                      TypeRef: i32
                      TypeRef: i32
                    TypeRef: void
                  BlockStatement
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseFunctionTypeInVariableTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                var x: (i32, i32) => void = 0;
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
                      Variable: x
                        FunctionType
                          Parameters
                            TypeRef: i32
                            TypeRef: i32
                          TypeRef: void
                        Literal: Integer = 0
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseAliasInterfaceTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point = {
                x: i32;
                y: i32;

                distance(Point): f32;
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: Point
                  AccessModifier: public
                  Interface
                    Properties
                      InterfaceProperty: x
                        TypeRef: i32
                      InterfaceProperty: y
                        TypeRef: i32
                    Methods
                      InterfaceMethod: distance
                        Parameters
                          TypeRef: Point
                        TypeRef: f32
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseAliasInterfaceTypeWithGettersSettersTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point = {
                x: i32 { public get; public set; }
                y: i32 { private get; private set; }

                distance(Point): f32;
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: Point
                  AccessModifier: public
                  Interface
                    Properties
                      InterfaceProperty: x
                        TypeRef: i32
                      InterfaceProperty: y
                        TypeRef: i32
                    Methods
                      InterfaceMethod: distance
                        Parameters
                          TypeRef: Point
                        TypeRef: f32
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseAliasInterfaceTypeWithGetOnlyTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point = {
                x: i32 { public get; }
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: Point
                  AccessModifier: public
                  Interface
                    Properties
                      InterfaceProperty: x
                        TypeRef: i32
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseAliasInterfaceTypeWithSetOnlyTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point = {
                x: i32 { public set; }
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: Point
                  AccessModifier: public
                  Interface
                    Properties
                      InterfaceProperty: x
                        TypeRef: i32
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseAliasInterfaceTypeMissingCloseBraceTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point = {
                x: i32;
                y: i32;

                distance(Point): f32;
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: Point
                  AccessModifier: public
                  Interface
                    Properties
                      InterfaceProperty: x
                        TypeRef: i32
                      InterfaceProperty: y
                        TypeRef: i32
                    Methods
                      InterfaceMethod: distance
                        Parameters
                          TypeRef: Point
                        TypeRef: f32
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(90, 7, 26).ToSpan()),
            "Expected '}'.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseAliasInterfaceTypeMissingPropertyTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point = {
                x: ;
                y: i32;

                distance(Point): f32;
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: Point
                  AccessModifier: public
                  Interface
                    Properties
                      InterfaceProperty: x
                        FakeType: <>_0
                      InterfaceProperty: y
                        TypeRef: i32
                    Methods
                      InterfaceMethod: distance
                        Parameters
                          TypeRef: Point
                        TypeRef: f32
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(47, 4, 8).ToSpan()),
            "Expected a type.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseAliasInterfaceTypeMissingPropertySemiColonTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point = {
                x: i32
                y: i32;

                distance(Point): f32;
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: Point
                  AccessModifier: public
                  Interface
                    Properties
                      InterfaceProperty: x
                        TypeRef: i32
                      InterfaceProperty: y
                        TypeRef: i32
                    Methods
                      InterfaceMethod: distance
                        Parameters
                          TypeRef: Point
                        TypeRef: f32
            """;

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(55, 5, 5).ToSpan()),
                "Expected '{'."),
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(55, 5, 5).ToSpan()),
                "Expected '}'.")
        };

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void ParseAliasInterfaceTypeMissingMethodReturnTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point = {
                x: i32;
                y: i32;

                distance(Point): ;
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: Point
                  AccessModifier: public
                  Interface
                    Properties
                      InterfaceProperty: x
                        TypeRef: i32
                      InterfaceProperty: y
                        TypeRef: i32
                    Methods
                      InterfaceMethod: distance
                        Parameters
                          TypeRef: Point
                        FakeType: <>_0
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(86, 7, 22).ToSpan()),
            "Expected a type.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseAliasInterfaceTypeMissingMethodColonTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point = {
                x: i32;
                y: i32;

                distance(Point) f64;
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: Point
                  AccessModifier: public
                  Interface
                    Properties
                      InterfaceProperty: x
                        TypeRef: i32
                      InterfaceProperty: y
                        TypeRef: i32
                    Methods
                      InterfaceMethod: distance
                        Parameters
                          TypeRef: Point
                        TypeRef: f64
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(85, 7, 21).ToSpan()),
            "Expected ':'.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseAliasInterfaceTypeMissingMethodSemiColonTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type Point = {
                x: i32;
                y: i32;

                distance(Point): f64
            }
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: Point
                  AccessModifier: public
                  Interface
                    Properties
                      InterfaceProperty: x
                        TypeRef: i32
                      InterfaceProperty: y
                        TypeRef: i32
                    Methods
                      InterfaceMethod: distance
                        Parameters
                          TypeRef: Point
                        TypeRef: f64
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(90, 8, 1).ToSpan()),
            "Expected ';'.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseArrayOfInterfacesTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type T = { x: i32; }[];
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  ArrayType
                    Interface
                      Properties
                        InterfaceProperty: x
                          TypeRef: i32
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void ParseDiscriminatedUnionTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T = { } | i32 | () => void;");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  DiscriminatedUnion
                    Types
                      Interface
                      TypeRef: i32
                      FunctionType
                        TypeRef: void
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseArrayOfDuTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type T = (i32 | null)[];
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  ArrayType
                    DiscriminatedUnion
                      Types
                        TypeRef: i32
                        TypeRef: null
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void ParseArrayInDuTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type T = null | i32[];
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  DiscriminatedUnion
                    Types
                      TypeRef: null
                      ArrayType
                        TypeRef: i32
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void ParseNullTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                var x: null = null;
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
                      Variable: x
                        TypeRef: null
                        Null
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseTupleTypeTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T = (i32, i32);");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  TupleType
                    Types
                      TypeRef: i32
                      TypeRef: i32
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseNestedTupleTypeTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T = ((i32, i32), i32);");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  TupleType
                    Types
                      TupleType
                        Types
                          TypeRef: i32
                          TypeRef: i32
                      TypeRef: i32
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseDuInTupleTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T = (bool | i32, () => void);");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  TupleType
                    Types
                      DiscriminatedUnion
                        Types
                          TypeRef: bool
                          TypeRef: i32
                      FunctionType
                        TypeRef: void
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseTupleInDuTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T = i32 | (bool, f64);");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  DiscriminatedUnion
                    Types
                      TypeRef: i32
                      TupleType
                        Types
                          TypeRef: bool
                          TypeRef: f64
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseTypeInParenTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T = (i32);");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  TypeRef: i32
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void TupleTypeMissingTest()
    {
        const string code = "namespace Test1;\n\npublic type T = (i32";

        var (tree, diagnostics) = Parse(code);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  TypeRef: i32
            """;

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(38, 3, 21).ToSpan()),
                "Expected ')'."),
            new Diagnostic(
                DiagnosticId.P0001MissingToken,
                DiagnosticSeverity.Error,
                new SourceLocation(file, new SourcePosition(38, 3, 21).ToSpan()),
                "Expected ';'.")
        };

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void FunctionWithTupleTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic main(): (i32, i32) { }");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Function: main
                  AccessModifier: public
                  TupleType
                    Types
                      TypeRef: i32
                      TypeRef: i32
                  BlockStatement
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseDuWithFunctionTypeAndDuTest()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T = i32 | (() => i32 | null);");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  DiscriminatedUnion
                    Types
                      TypeRef: i32
                      FunctionType
                        DiscriminatedUnion
                          Types
                            TypeRef: i32
                            TypeRef: null
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseDuWithFunctionTypeAndDu2Test()
    {
        var (tree, diagnostics) = Parse("namespace Test1;\n\npublic type T = i32 | (() => i32) | null;");

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  DiscriminatedUnion
                    Types
                      TypeRef: i32
                      FunctionType
                        TypeRef: i32
                      TypeRef: null
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseArrayOfTypesTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type T = i32[];
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  ArrayType
                    TypeRef: i32
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void ParseArrayOfTuplesTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type T = (i32, null)[];
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  ArrayType
                    TupleType
                      Types
                        TypeRef: i32
                        TypeRef: null
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void ParseArrayOfArraysTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type T = i32[][];
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  ArrayType
                    ArrayType
                      TypeRef: i32
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void ParseArrayInReturnOfFunctionTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type T = () => i32[];
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  FunctionType
                    ArrayType
                      TypeRef: i32
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void ParseArrayOfGenericsTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type List<T> { }

            public type Test = List<i32>[];
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: List
                  AccessModifier: public
                  Generic Arguments
                    TypeRef: T
                TypeAlias: Test
                  AccessModifier: public
                  ArrayType
                    GenericApplication
                      TypeRef: List
                      TypeArguments
                        TypeRef: i32
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
    }

    [Test]
    public void ParseArrayInGenericTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public type List<T> { }

            public type Test = List<i32[]>;
            """);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: List
                  AccessModifier: public
                  Generic Arguments
                    TypeRef: T
                TypeAlias: Test
                  AccessModifier: public
                  GenericApplication
                    TypeRef: List
                    TypeArguments
                      ArrayType
                        TypeRef: i32
            """;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
    }
}