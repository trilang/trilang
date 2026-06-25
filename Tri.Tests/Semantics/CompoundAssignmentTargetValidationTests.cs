using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Semantics;

public class CompoundAssignmentTargetValidationTests
{
    [Test]
    public void CompoundAssignmentForParameterTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(a: i32): void {
                a += 1;
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

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void CompoundAssignmentForVariableTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): void {
                var a: i32 = 0;
                a += 1;
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

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void CompoundAssignmentForPropertyTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                x: i32;
            }

            public main(p: Point): void {
                p.x += 1;
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

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void CompoundAssignmentForArrayTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(a: i32[]): void {
                a[0] += 1;
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

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void InvalidCompoundAssignmentForNumberTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): void {
                1 += 1;
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
            DiagnosticId.S0033InvalidOperandForCompoundAssignment,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(44, 4, 5), new SourcePosition(50, 4, 11))),
            "Cannot apply a compound assignment to this expression.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }
}