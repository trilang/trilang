using Trilang;
using Trilang.Compilation;
using Trilang.Compilation.Diagnostics;
using Trilang.Lower;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;
using Trilang.Semantics.Model;

namespace Tri.Tests;

public static class Helpers
{
    public static SourceFile CreateFile(string content)
        => new SourceFile("test.tri", content);

    public static (SyntaxTree, DiagnosticCollection) ParseFile(SourceFile file)
    {
        var (project, diagnostics) = Parse(file);
        var tree = project.SourceFiles.Single().SyntaxTree!;

        return (tree, diagnostics);
    }

    public static (Project, DiagnosticCollection) Parse(params IEnumerable<SourceFile> files)
    {
        var diagnostics = new DiagnosticCollection();
        var project = new Project(
            "test",
            "test",
            files.Select(x => new CompilationUnit(x)).ToList(),
            []);

        foreach (var compilationUnit in project.SourceFiles)
        {
            var parser = new Parser();
            var parserOptions = new ParserOptions(diagnostics);
            parser.Parse(compilationUnit, parserOptions);
        }

        return (project, diagnostics);
    }

    public static (SemanticTree, DiagnosticCollection, CompilationContext) Lower(SourceFile file)
        => Lower(file, []);

    public static (SemanticTree, DiagnosticCollection, CompilationContext) Lower(
        SourceFile file,
        IReadOnlyList<string> directives)
    {
        var (project, diagnostics) = Parse(file);

        var directivesSet = directives.ToHashSet();
        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);

        var semantic = new SemanticAnalyzer();
        var (_, cfgs) = semantic.Analyze(
            project,
            new SemanticAnalysisOptions(directivesSet, diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var lowering = new Trilang.Lower.Lowering(compilationContext.BuiltInTypes);
        lowering.Lower(semanticTree, new LoweringOptions(directivesSet, cfgs));

        return (semanticTree, diagnostics, compilationContext);
    }
}