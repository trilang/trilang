using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Semantics;

public class VariableUsedBeforeDeclaredTests
{
    [Test]
    public void VariableUsedAfterDeclarationTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): void {
                var a: i32 = 1;
                a;
            }
            """);
        var (tree, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();

        Assert.That(
            () => semantic.Analyze(
                tree,
                new SemanticAnalysisOptions(
                    new HashSet<string>(),
                    diagnostics,
                    compilationContext)),
            Throws.Nothing);
    }

    [Test]
    public void ParameterUsedAfterDeclarationTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(a: i32): void {
                a;
            }
            """);
        var (tree, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();

        Assert.That(
            () => semantic.Analyze(
                tree,
                new SemanticAnalysisOptions(
                    new HashSet<string>(),
                    diagnostics,
                    compilationContext)),
            Throws.Nothing);
    }

    [Test]
    public void VariableUsedBeforeDeclarationTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): void {
                a;
                var a: i32 = 1;
            }
            """);
        var (tree, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            tree,
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                diagnostics,
                compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0020VariableUsedBeforeDeclaration,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(44, 4, 5), new SourcePosition(45, 4, 6))),
            "The 'a' variable is used before its declaration.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void VariableInBlockUsedBeforeDeclarationTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): void {
                {
                    a;
                }
                var a: i32 = 1;
            }
            """);
        var (tree, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            tree,
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                diagnostics,
                compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0020VariableUsedBeforeDeclaration,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(54, 5, 9), new SourcePosition(55, 5, 10))),
            "The 'a' variable is used before its declaration.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void VariableInDifferentBlocksTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): void {
                {
                    var a: i32 = 1;
                }
                {
                    a;
                }
            }
            """);
        var (tree, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            tree,
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                diagnostics,
                compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0008UnknownMember,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(90, 8, 9), new SourcePosition(91, 8, 10))),
            "Unknown symbol: 'a'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void VariableInDeclaredInDifferentFunctionTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(): void {
                var a: i32 = 1;
            }

            public main(): void {
                a;
            }
            """);
        var (tree, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            tree,
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                diagnostics,
                compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0008UnknownMember,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(89, 8, 5), new SourcePosition(90, 8, 6))),
            "Unknown symbol: 'a'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void VariableInParentScopeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(): i32 {
                var a: i32 = 1;
                {
                    return a;
                }
            }
            """);
        var (tree, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();

        Assert.That(
            () => semantic.Analyze(
                tree,
                new SemanticAnalysisOptions(
                    new HashSet<string>(),
                    diagnostics,
                    compilationContext)),
            Throws.Nothing);
    }
}