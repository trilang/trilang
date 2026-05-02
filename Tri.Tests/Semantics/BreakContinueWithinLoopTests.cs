using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Semantics;

public class BreakContinueWithinLoopTests
{
    [Test]
    public void BreakIsNotInLoopTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(): void {
                break;
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
            DiagnosticId.S0012BreakOutsideLoop,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(44, 4, 5), new SourcePosition(50, 4, 11))),
            "The 'break' keyword can only be used within a loop.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ContinueIsNotInLoopTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(): void {
                continue;
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
            DiagnosticId.S0013ContinueOutsideLoop,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(44, 4, 5), new SourcePosition(53, 4, 14))),
            "The 'continue' keyword can only be used within a loop.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void BreakInNestedLoopTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(): void {
                while (true) {
                    while (false) {
                        break;
                    }
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
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                diagnostics,
                compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var breakNode = semanticTree.Find<Break>();
        var loop = semanticTree.Where<While>().Last();
        Assert.That(breakNode, Is.Not.Null);
        Assert.That(breakNode.LoopNode, Is.EqualTo(loop));
    }

    [Test]
    public void ContinueInNestedLoopTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(): void {
                while (true) {
                    while (false) {
                        continue;
                    }
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
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                diagnostics,
                compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var continueNode = semanticTree.Find<Continue>();
        var loop = semanticTree.Where<While>().Last();
        Assert.That(continueNode, Is.Not.Null);
        Assert.That(continueNode.LoopNode, Is.EqualTo(loop));
    }
}