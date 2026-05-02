using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Semantics;

public class PrivateInterfacePropertiesTests
{
    [Test]
    public void PrivateGetterTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test = {
                x: i32 { private get; }
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
            DiagnosticId.S0022InterfacePropertyCantBePrivate,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(43, 4, 5), new SourcePosition(66, 4, 28))),
            "The getter of the interface property 'x' cannot be private.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void PrivateSetterTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test = {
                x: i32 { private set; }
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
            DiagnosticId.S0022InterfacePropertyCantBePrivate,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(43, 4, 5), new SourcePosition(66, 4, 28))),
            "The setter of the interface property 'x' cannot be private.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void PrivateGetterAndSetterTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test = {
                x: i32 { private get; private set; }
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

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.S0022InterfacePropertyCantBePrivate,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    file,
                    new SourceSpan(new SourcePosition(43, 4, 5), new SourcePosition(79, 4, 41))),
                "The getter of the interface property 'x' cannot be private."),
            new Diagnostic(
                DiagnosticId.S0022InterfacePropertyCantBePrivate,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    file,
                    new SourceSpan(new SourcePosition(43, 4, 5), new SourcePosition(79, 4, 41))),
                "The setter of the interface property 'x' cannot be private.")
        };

        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }
}