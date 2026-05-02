using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Semantics;

public class CyclicAliasTests
{
    [Test]
    public void RecursiveTypeAliasTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test = Test;
            """);
        var (project, diagnostics) = Parse([file]);

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

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var testAlias = test1Ns.FindType("Test");

        var diagnostic = new Diagnostic(
            DiagnosticId.S0001CyclicTypeAlias,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(18, 3, 1), new SourcePosition(42, 3, 25))),
            "The cyclic type alias detected: 'Test'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
        Assert.That(testAlias, Is.Not.Null);
        Assert.That(testAlias.IsInvalid, Is.True);
    }

    [Test]
    public void RecursiveTypeAliasTest2()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test1 = Test2;
            public type Test2 = Test1;
            """);
        var (project, diagnostics) = Parse([file]);

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

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var test1Alias = test1Ns.FindType("Test1");
        var test2Alias = test1Ns.FindType("Test2");

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.S0001CyclicTypeAlias,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    file,
                    new SourceSpan(new SourcePosition(18, 3, 1), new SourcePosition(44, 3, 27))),
                "The cyclic type alias detected: 'Test1'."),
            new Diagnostic(
                DiagnosticId.S0001CyclicTypeAlias,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    file,
                    new SourceSpan(new SourcePosition(45, 4, 1), new SourcePosition(71, 4, 27))),
                "The cyclic type alias detected: 'Test2'.")
        };

        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
        Assert.That(test1Alias, Is.Not.Null);
        Assert.That(test1Alias.IsInvalid, Is.True);
        Assert.That(test2Alias, Is.Not.Null);
        Assert.That(test2Alias.IsInvalid, Is.True);
    }

    [Test]
    public void RecursiveTypesInDifferentNamespacesTest()
    {
        var file1 = new SourceFile(
            "file1.tri",
            """
            namespace NS1;

            use NS2;

            public type Test1 = Test2;
            """);
        var file2 = new SourceFile(
            "file2.tri",
            """
            namespace NS2;

            use NS1;

            public type Test2 = Test1;
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

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.S0001CyclicTypeAlias,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    file1,
                    new SourceSpan(new SourcePosition(26, 5, 1), new SourcePosition(52, 5, 27))),
                "The cyclic type alias detected: 'Test1'."),
            new Diagnostic(
                DiagnosticId.S0001CyclicTypeAlias,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    file2,
                    new SourceSpan(new SourcePosition(26, 5, 1), new SourcePosition(52, 5, 27))),
                "The cyclic type alias detected: 'Test2'.")
        };

        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }
}