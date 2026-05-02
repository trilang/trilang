using Trilang;
using Trilang.Compilation.Diagnostics;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Parsing;

public class ParseNamespaceTests
{
    [Test]
    public void ParseNamespaceTest()
    {
        var file = CreateFile(
            """
            namespace Test;

            public main(): void { }
            """);
        var (tree, diagnostics) = ParseFile(file);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test
              Declarations
                Function: main
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseMultiPartNamespaceTest()
    {
        var file = CreateFile(
            """
            namespace Test.SubNamespace.SubSubNamespace;

            public main(): void { }
            """);
        var (tree, diagnostics) = ParseFile(file);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test.SubNamespace.SubSubNamespace
              Declarations
                Function: main
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseUseNamespaceTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            use Test;

            public main(): void { }
            """);
        var (tree, diagnostics) = ParseFile(file);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              UseNodes
                Use
                  Parts: Test
              Declarations
                Function: main
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseUseMultipartNamespaceTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            use Test.SubNamespace.SubSubNamespace;

            public main(): void { }
            """);
        var (tree, diagnostics) = ParseFile(file);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              UseNodes
                Use
                  Parts: Test.SubNamespace.SubSubNamespace
              Declarations
                Function: main
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseUseInvalidNamespaceTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            use Test.1.SubSubNamespace;

            public main(): void { }
            """);
        var (tree, diagnostics) = ParseFile(file);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              UseNodes
                Use
                  Parts: Test.<invalid_namespace>.SubSubNamespace
              Declarations
                Function: main
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0015ExpectedNamespacePart,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(27, 3, 10), new SourcePosition(28, 3, 11))),
            "Expected a namespace part.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ParseNamespaceAndUseTest()
    {
        var file = CreateFile(
            """
            namespace Test;

            use OtherNamespace;

            public main(): void { }
            """);
        var (tree, diagnostics) = ParseFile(file);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test
              UseNodes
                Use
                  Parts: OtherNamespace
              Declarations
                Function: main
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseFullyQualifiedNameTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type T = Some.NS.TestType;
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
                  TypeRef: Some.NS.TestType
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseUsePackagePrefixTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            use std::io;

            public main(): void { }
            """);
        var (tree, diagnostics) = ParseFile(file);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              UseNodes
                Use
                  Package: std
                  Parts: io
              Declarations
                Function: main
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseUsePackageMultipartNamespaceTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            use mypkg::foo.bar;

            public main(): void { }
            """);
        var (tree, diagnostics) = ParseFile(file);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              UseNodes
                Use
                  Package: mypkg
                  Parts: foo.bar
              Declarations
                Function: main
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
            """;

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ParseUseInvalidPackagePrefixTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            use ::foo;

            public main(): void { }
            """);
        var (tree, diagnostics) = ParseFile(file);

        const string expected =
            """
            SyntaxTree
              Namespace
                Parts: Test1
              UseNodes
                Use
                  Parts: <invalid_namespace>
              Declarations
                Function: main
                  AccessModifier: public
                  TypeRef: void
                  BlockStatement
            """;

        var diagnostic = new Diagnostic(
            DiagnosticId.P0016ExpectedNamespace,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(22, 3, 5), new SourcePosition(27, 3, 10))),
            "Expected a namespace.");

        Assert.That(tree.Dump(), Is.EqualTo(expected).NoClip);
        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }
}