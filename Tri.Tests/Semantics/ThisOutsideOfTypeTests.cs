using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Semantics;

public class ThisOutsideOfTypeTests
{
    [Test]
    public void ThisInConstructorTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                public constructor() {
                    this;
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
    public void ThisInMethodTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                public toString(): void {
                    this;
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
    public void ThisInFunctionTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): void {
                this;
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
            DiagnosticId.S0017ThisOutsideOfType,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(44, 4, 5), new SourcePosition(48, 4, 9))),
            "The 'this' keyword can only be used within a type.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }
}