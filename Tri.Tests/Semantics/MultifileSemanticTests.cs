using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Semantics;

public class MultifileSemanticTests
{
    [Test]
    public void UseTypeFromOtherFileTest()
    {
        var (project, diagnostics) = Parse(
            new SourceFile(
                "point.tri",
                """
                namespace Test1;

                public type Point {}
                """),
            new SourceFile(
                "test.tri",
                """
                namespace Test1;

                public main(): void {
                    var p: Point = new Point();
                }
                """));

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

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void ForwardDeclareTypeFromOtherFileTest()
    {
        var (project, diagnostics) = Parse([
            new SourceFile(
                "test.tri",
                """
                namespace Test1;

                public main(): void {
                    var p: Point = new Point();
                }
                """),
            new SourceFile(
                "point.tri",
                """
                namespace Test1;

                public type Point {}
                """)
        ]);

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

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void DuplicateTypeDeclarationTest()
    {
        var file1 = new SourceFile(
            "point1.tri",
            """
            namespace Test1;

            public type Point {}
            """);
        var file2 = new SourceFile(
            "point2.tri",
            """
            namespace Test1;

            public type Point {}
            """);
        var (project, diagnostics) = Parse([file1, file2]);

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
            DiagnosticId.S0002AlreadyDefined,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file2,
                new SourceSpan(new SourcePosition(18, 3, 1), new SourcePosition(38, 3, 21))),
            "The 'Point' type is already defined.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }
}