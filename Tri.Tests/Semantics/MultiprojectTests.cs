using Trilang;
using Trilang.Compilation;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class MultiprojectTests
{
    [Test]
    public void AnalyzeMultipleProjectsTest()
    {
        var diagnostics = new DiagnosticCollection();
        var project3 = new Project(
            "test3",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test3;

                    public type MyType = i32;
                    """))
            ],
            []);

        var project2 = new Project(
            "test2",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test2;

                    public type MyType = i32;
                    """))
            ],
            [project3.ToProjectInfo()]);

        var project1 = new Project(
            "test1",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test1;

                    use test2::Test2;

                    public test(p: MyType): void {
                    }
                    """))
            ],
            [project2.ToProjectInfo()]);

        var projects = (Project[])[project3, project2, project1];
        foreach (var compilationUnit in projects.SelectMany(x => x.SourceFiles))
        {
            var parser = new Parser();
            var parserOptions = new ParserOptions(diagnostics);
            parser.Parse(compilationUnit, parserOptions);
        }

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var compilationContexts = new List<CompilationContext>();
        var builtInTypes = new BuiltInTypes();
        foreach (var project in projects)
        {
            var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
            var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
            foreach (var dependencyInfo in project.Dependencies)
            {
                var dependency = compilationContexts.Single(x => x.CurrentPackage!.Name == dependencyInfo.Name);
                rootNamespace.AddImported(dependency.RootNamespace);
                compilationContext.AddPackage(dependency.CurrentPackage!);
            }

            var semanticAnalyzer = new SemanticAnalyzer();
            semanticAnalyzer.Analyze(
                project,
                new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

            compilationContexts.Add(compilationContext);
        }

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void MissingProjectTest()
    {
        var diagnostics = new DiagnosticCollection();
        var project1 = new Project(
            "test1",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test1;

                    use test2::Test2;

                    public test(p: MyType): void {
                    }
                    """))
            ],
            []);

        foreach (var compilationUnit in project1.SourceFiles)
        {
            var parser = new Parser();
            var parserOptions = new ParserOptions(diagnostics);
            parser.Parse(compilationUnit, parserOptions);
        }

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semanticAnalyzer = new SemanticAnalyzer();
        semanticAnalyzer.Analyze(
            project1,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.S0029UnknownPackage,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    project1.SourceFiles.Single().File,
                    new SourceSpan(new SourcePosition(18, 3, 1), new SourcePosition(35, 3, 18))),
                "Unknown package: test2."),
            new Diagnostic(
                DiagnosticId.S0003UnknownType,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    project1.SourceFiles.Single().File,
                    new SourceSpan(new SourcePosition(52, 5, 16), new SourcePosition(58, 5, 22))),
                "Unknown type: 'MyType'."),
        };
        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void AnonymousTypeIsDefinedOnlyInOneRootNamespaceTest()
    {
        var diagnostics = new DiagnosticCollection();
        var project2 = new Project(
            "test2",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test2;

                    public type MyType = { }
                    """))
            ],
            []);

        var project1 = new Project(
            "test1",
            string.Empty,
            [
                new CompilationUnit(new SourceFile(
                    "test.tri",
                    """
                    namespace Test1;

                    public type MyType = { }
                    """))
            ],
            [project2.ToProjectInfo()]);

        var projects = (Project[])[project2, project1];
        foreach (var compilationUnit in projects.SelectMany(x => x.SourceFiles))
        {
            var parser = new Parser();
            var parserOptions = new ParserOptions(diagnostics);
            parser.Parse(compilationUnit, parserOptions);
        }

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var compilationContexts = new List<CompilationContext>();
        var builtInTypes = new BuiltInTypes();
        foreach (var project in projects)
        {
            var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
            var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
            foreach (var dependencyInfo in project.Dependencies)
            {
                var dependency = compilationContexts.Single(x => x.CurrentPackage!.Name == dependencyInfo.Name);
                rootNamespace.AddImported(dependency.RootNamespace);
                compilationContext.AddPackage(dependency.CurrentPackage!);
            }

            var semanticAnalyzer = new SemanticAnalyzer();
            semanticAnalyzer.Analyze(
                project,
                new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

            compilationContexts.Add(compilationContext);
        }

        var mainPackage = compilationContexts.Last();
        var test1Ns = mainPackage.FindNamespace("test1", ["Test1"]).Namespace!;
        var test2Ns = mainPackage.FindNamespace("test2", ["Test2"]).Namespace!;
        var myType1 = (AliasMetadata)test1Ns.FindType("MyType")!;
        var myType2 = (AliasMetadata)test2Ns.FindType("MyType")!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(myType1, Is.Not.EqualTo(myType2));
        Assert.That(myType1.Type, Is.SameAs(myType2.Type));
    }
}