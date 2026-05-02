using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Semantics;

public class NotImplementedInterfaceTests
{
    [Test]
    public void EverythingIsImplementedInTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Interface1 = {
                x: i32;
                toString(): string;
            }

            public type Test : Interface1 {
                x: i32;
                public toString(): string {
                    return "Hello, World!";
                }
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();

        Assert.That(
            () => semantic.Analyze(
                project,
                new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext)),
            Throws.Nothing);
    }

    [Test]
    public void NotImplementedPropertyTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Interface1 = {
                x: i32;
                toString(): string;
            }

            public type Test : Interface1 {
                public toString(): string {
                    return "Hello, World!";
                }
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0021MemberIsNotImplemented,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(84, 8, 1), new SourcePosition(187, 12, 2))),
            "The 'x' property is not implemented.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ImplementPropertyWithIncorrectTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Interface1 = {
                x: i32;
                toString(): string;
            }

            public type Test : Interface1 {
                x: i8;
                public toString(): string {
                    return "Hello, World!";
                }
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0021MemberIsNotImplemented,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(120, 9, 5), new SourcePosition(126, 9, 11))),
            "The 'x' property is not of the correct type. Expected 'i8', got 'i32'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void NotImplementedMethodTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Interface1 = {
                x: i32;
                toString(): string;
            }

            public type Test : Interface1 {
                x: i32;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0021MemberIsNotImplemented,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(84, 8, 1),
                    new SourcePosition(129, 10, 2))),
            "The 'toString' method is not implemented.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ImplementMethodWithIncorrectReturnTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Interface1 = {
                x: i32;
                toString(): string;
            }

            public type Test : Interface1 {
                x: i32;
                public toString(): i32 {
                    return 1;
                }
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0021MemberIsNotImplemented,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(132, 10, 5),
                    new SourcePosition(180, 12, 6))),
            "The 'toString' method is not of the correct type. Expected '() => string', got '() => i32'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ImplementMethodWithIncorrectParametersTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Interface1 = {
                x: i32;
                toString(): string;
            }

            public type Test : Interface1 {
                x: i32;
                public toString(a: i32): string {
                    return "Hello, World!";
                }
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0021MemberIsNotImplemented,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(132, 10, 5),
                    new SourcePosition(203, 12, 6))
            ),
            "The 'toString' method is not of the correct type. Expected '() => string', got '(i32) => string'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void TypeImplementsMethodAsPrivateTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Interface1 = {
                method(): void;
            }

            public type Test : Interface1 {
                private method(): void {
                }
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0021MemberIsNotImplemented,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(104, 8, 5),
                    new SourcePosition(134, 9, 6))),
            "The implementation of the interface method 'method' is not public.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void TypeImplementsGetterAsPrivateTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Interface1 = {
                x: i32 { public get; public set; }
            }

            public type Test : Interface1 {
                x: i32 { private get; public set; }
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0021MemberIsNotImplemented,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(123, 8, 5),
                    new SourcePosition(158, 8, 40))),
            "The implementation of an interface property getter 'x' cannot be private.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void TypeImplementsSetterAsPrivateTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Interface1 = {
                x: i32 { public get; public set; }
            }

            public type Test : Interface1 {
                x: i32 { public get; private set; }
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0021MemberIsNotImplemented,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(123, 8, 5),
                    new SourcePosition(158, 8, 40))),
            "The implementation of an interface property setter 'x' cannot be private.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }
}