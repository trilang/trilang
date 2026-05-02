using Trilang;
using Trilang.Compilation.Diagnostics;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Parsing;

public class ParseGenericTypeTests
{
    [Test]
    public void ParseGenericTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type List<T> { }
            """);
        var (tree, diagnostics) = ParseFile(file);

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
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseGenericTypeMissingTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type List<> { }
            """);
        var (tree, diagnostics) = ParseFile(file);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                Type: List
                  AccessModifier: public
                  Generic Arguments
                    FakeType: <>_0
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(35, 3, 18).ToSpan()),
            "Expected a type.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseGenericTypeMissingSecondTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type List<T,> { }
            """);
        var (tree, diagnostics) = ParseFile(file);

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
                    FakeType: <>_0
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(37, 3, 20).ToSpan()),
            "Expected a type.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseGenericTypeMissingGreaterTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type List<T { }
            """);
        var (tree, diagnostics) = ParseFile(file);

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
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(37, 3, 20).ToSpan()),
            "Expected '>'.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseAliasToGenericTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type T = List<i32, bool>;
            """);
        var (tree, diagnostics) = ParseFile(file);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  GenericApplication
                    TypeRef: List
                    TypeArguments
                      TypeRef: i32
                      TypeRef: bool
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseNestedGenericTypeAliasTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type T = List<i32, List<bool>>;
            """);
        var (tree, diagnostics) = ParseFile(file);
        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  GenericApplication
                    TypeRef: List
                    TypeArguments
                      TypeRef: i32
                      GenericApplication
                        TypeRef: List
                        TypeArguments
                          TypeRef: bool
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseAliasToGenericTypeMissingTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type T = List<>;
            """);
        var (tree, diagnostics) = ParseFile(file);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  GenericApplication
                    TypeRef: List
                    TypeArguments
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
    public void ParseAliasToGenericTypeMissingSecondTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type T = List<i32, >;
            """);
        var (tree, diagnostics) = ParseFile(file);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  GenericApplication
                    TypeRef: List
                    TypeArguments
                      TypeRef: i32
                      FakeType: <>_0
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(44, 3, 27).ToSpan()),
            "Expected a type.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseAliasToGenericTypeMissingCloseAngleBracketTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type T = List<i32;
            """);
        var (tree, diagnostics) = ParseFile(file);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              Declarations
                TypeAlias: T
                  AccessModifier: public
                  GenericApplication
                    TypeRef: List
                    TypeArguments
                      TypeRef: i32
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(42, 3, 25).ToSpan()),
            "Expected '>'.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseGenericTypeAliasTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type T<T1, T2> = T1 | T2;
            """);
        var (tree, diagnostics) = ParseFile(file);

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
                      TypeRef: T1
                      TypeRef: T2
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseGenericTypeAliasMissingTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type T<> = T1 | T2;
            """);
        var (tree, diagnostics) = ParseFile(file);

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
                      TypeRef: T1
                      TypeRef: T2
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(32, 3, 15).ToSpan()),
            "Expected a type.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseGenericTypeAliasMissingSecondTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type T<T1, > = T1 | T2;
            """);
        var (tree, diagnostics) = ParseFile(file);

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
                      TypeRef: T1
                      TypeRef: T2
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0003ExpectedType,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(36, 3, 19).ToSpan()),
            "Expected a type.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseGenericTypeAliasMissingCloseAngleBracketTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type T<T1, T2 = T1 | T2;
            """);
        var (tree, diagnostics) = ParseFile(file);

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
                      TypeRef: T1
                      TypeRef: T2
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0001MissingToken,
            DiagnosticSeverity.Error,
            new SourceLocation(file, new SourcePosition(39, 3, 22).ToSpan()),
            "Expected '>'.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }
}