using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Semantics;

public class CheckStaticAndInstanceMembersAccessTests
{
    [Test]
    public void AccessNotStaticMethodOnTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test {
                public s(): void { }
            }

            public func(): void {
                Test.s();
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                diagnostics,
                compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0019InstanceMethodAsStatic,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(91, 8, 5), new SourcePosition(97, 8, 11))),
            "The instance method 's' cannot be called as a static one.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void AccessStaticMethodOnInstanceTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test {
                public static s(): void { }
            }

            public func(a: Test): void {
                a.s();
            }
            """);
        var (project, diagnostics) = Parse(file);

       var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                diagnostics,
                compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0018StaticMethodAsInstance,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(105, 8, 5), new SourcePosition(108, 8, 8))),
            "The static method 's' cannot be called as an instance one.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void AccessInstanceMethodWithThisTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test {
                public method1(): void { }

                public method2(): void {
                    this.method1();
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
                new SemanticAnalysisOptions(
                    new HashSet<string>(),
                    diagnostics,
                    compilationContext)),
            Throws.Nothing);
    }

    [Test]
    public void AccessStaticOnInvalidTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test = Test;

            public func(): void {
                Test.s();
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                diagnostics,
                compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0001CyclicTypeAlias,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(18, 3, 1), new SourcePosition(42, 3, 25))),
            "The cyclic type alias detected: 'Test'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }
}