using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Semantics;

public class MissingReturnStatementTests
{
    [Test]
    public void MissingReturnInFunctionTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(): i32 { }
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
            DiagnosticId.S0015NotAllPathsReturnValue,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(18, 3, 1), new SourcePosition(40, 3, 23))),
            "Not all paths return a value.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void MissingReturnInFunctionWithIfTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(): i32 {
                if (false) {
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
            DiagnosticId.S0015NotAllPathsReturnValue,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(18, 3, 1), new SourcePosition(81, 7, 2))),
            "Not all paths return a value.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ValidReturnInFunctionWithIfTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(): i32 {
                if (false) {
                    return 1;
                }

                return 0;
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
    public void ValidReturnInFunctionWithIfElseTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(): i32 {
                if (false) {
                    return 1;
                } else {
                    return 0;
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
    public void MissingReturnInFunctionWithWhileTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(): i32 {
                while (false) {
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
            DiagnosticId.S0015NotAllPathsReturnValue,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(18, 3, 1), new SourcePosition(84, 7, 2))),
            "Not all paths return a value.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ValidReturnInFunctionWithWhileTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(): i32 {
                while (false) { }

                return 0;
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
    public void ValidReturnInVoidFunctionTest()
    {
        var file = CreateFile(
            "public test(): void { }");
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
}