using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Semantics;

public class ThisInStaticMethodsTests
{
    [Test]
    public void ThisInStaticMethodsTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test {
                public static test(): Test {
                    return this;
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

        var diagnostic = new Diagnostic(
            DiagnosticId.S0016ThisInStaticMethod,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(85, 5, 16), new SourcePosition(89, 5, 20))),
            "The 'this' keyword cannot be used in a static method.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }
}