using System.Diagnostics;
using Trilang.Compilation.Diagnostics;
using Trilang.IntermediateRepresentation;
using Trilang.Lower;
using Trilang.Metadata;
using Trilang.OutputFormats.Elf;
using Trilang.Parsing;
using Trilang.Semantics;
using Trilang.Semantics.Passes.ControlFlow;

namespace Trilang.Compilation;

public class Compiler
{
    public void Compile(CompilerOptions options)
    {
        var diagnostics = new DiagnosticCollection();
        var projectLoader = new ProjectLoader(diagnostics);
        var projects = projectLoader.LoadInfo(options.Path);

        if (diagnostics.HasErrors)
        {
            PrintErrors(diagnostics);

            return;
        }

        var builtInTypes = new BuiltInTypes();
        var compilationContexts = new List<CompilationContext>();
        var functions = new List<IrFunction>();

        foreach (var projectInfo in projects)
        {
            var project = projectLoader.LoadProject(projectInfo);

            var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
            var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
            foreach (var dependencyInfo in project.Dependencies)
            {
                // TODO: handle two projects with the same name in different dependencies
                var dependency = compilationContexts.Single(x => x.CurrentPackage!.Name == dependencyInfo.Name);
                rootNamespace.AddImported(dependency.RootNamespace);
                compilationContext.AddPackage(dependency.CurrentPackage!);
            }

            Parse(diagnostics, project);

            var (_, controlFlowGraphs) = SemanticAnalysis(project, diagnostics, compilationContext, options);

            Lower(compilationContext, project, controlFlowGraphs, options);

            functions.AddRange(GenerateIr(compilationContext, project, options));

            Debug.Assert(compilationContext.CurrentPackage is not null);
            compilationContexts.Add(compilationContext);
        }

        if (diagnostics.HasErrors)
        {
            PrintErrors(diagnostics);

            return;
        }

        PrintIr(options, functions);
        OutputBinary(options);
    }

    private void PrintErrors(DiagnosticCollection diagnostics)
    {
        foreach (var diagnostic in diagnostics.Diagnostics)
        {
            // $"{diagnostic.File}({span}): {diagnostic.Severity} {diagnostic.Id} - {diagnostic.Message}"
            var originalColor = Console.ForegroundColor;

            Console.ForegroundColor = ConsoleColor.Blue;

            var location = diagnostic.Location;
            if (location is not null)
            {
                var (start, end) = location.Span;
                var span = start != end
                    ? $"{start.Line}:{start.Column} - {end.Line}:{end.Column}"
                    : $"{start.Line}:{start.Column}";

                Console.Write($"{location.File}({span})");
            }

            Console.ForegroundColor = originalColor;
            Console.Write(": ");

            Console.ForegroundColor = diagnostic.Severity switch
            {
                DiagnosticSeverity.Info => originalColor,
                DiagnosticSeverity.Warning => ConsoleColor.Yellow,
                DiagnosticSeverity.Error => ConsoleColor.Red,
                _ => throw new ArgumentOutOfRangeException(),
            };
            Console.WriteLine($"{diagnostic.Severity} {diagnostic.Id} - {diagnostic.Message}");
            Console.ForegroundColor = originalColor;

            // TODO: print source code with error
        }
    }

    private void Parse(DiagnosticCollection diagnostics, Project project)
    {
        var parser = new Parser();

        foreach (var compilationUnit in project.SourceFiles)
        {
            var parserOptions = new ParserOptions(diagnostics);
            parser.Parse(compilationUnit, parserOptions);
        }
    }

    private SemanticAnalysisResult SemanticAnalysis(
        Project project,
        DiagnosticCollection diagnostics,
        CompilationContext compilationContext,
        CompilerOptions options)
    {
        var semanticOptions = new SemanticAnalysisOptions(options.Directives, diagnostics, compilationContext);
        var semantic = new SemanticAnalyzer();
        var semanticResult = semantic.Analyze(project, semanticOptions);

        return semanticResult;
    }

    private static void Lower(
        CompilationContext compilationContext,
        Project project,
        ControlFlowGraphMap controlFlowGraphs,
        CompilerOptions options)
    {
        var lowering = new Lowering(compilationContext.BuiltInTypes);
        foreach (var compilationUnit in project.SourceFiles)
            lowering.Lower(
                compilationUnit.SemanticTree!,
                new LoweringOptions(options.Directives, controlFlowGraphs));
    }

    private IReadOnlyList<IrFunction> GenerateIr(
        CompilationContext compilationContext,
        Project project,
        CompilerOptions options)
    {
        var semanticTrees = project.SourceFiles.Select(x => x.SemanticTree!);
        var ir = new IrGenerator(options.Directives, compilationContext);
        var functions = ir.Generate(semanticTrees);

        return functions;
    }

    private void PrintIr(CompilerOptions options, IReadOnlyList<IrFunction> functions)
    {
        if (!options.PrintIr)
            return;

        foreach (var function in functions)
            Console.WriteLine(function);
    }

    private void OutputBinary(CompilerOptions options)
    {
        if (options.OperatingSystem == CompilerOptionOs.Linux)
        {
            var elfWriter = new ElfWriter();

            elfWriter.Write(new ElfOptions(
                options.OutputPath,
                options.Platform switch
                {
                    CompilerOptionPlatform.X64 => ElfInstructionSet.X86_64,
                    CompilerOptionPlatform.Arm64 => ElfInstructionSet.Arm64,
                    _ => throw new ArgumentOutOfRangeException(),
                }
            ));
        }
        else if (options.OperatingSystem == CompilerOptionOs.MacOs)
        {
            throw new NotImplementedException();
        }
        else
        {
            throw new Exception("Unsupported operating system.");
        }
    }
}