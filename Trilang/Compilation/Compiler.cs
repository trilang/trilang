using Trilang.Compilation.Diagnostics;
using Trilang.IntermediateRepresentation;
using Trilang.Lexing;
using Trilang.Lower;
using Trilang.OutputFormats.Elf;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Trilang.Compilation;

public class Compiler
{
    public void Compile(CompilerOptions options)
    {
        var diagnostics = new DiagnosticCollection();
        var lexer = new Lexer();
        var parser = new Parser();
        var semantic = new SemanticAnalysis();
        var semanticOptions = new SemanticAnalysisOptions(
            options.Directives,
            new SemanticDiagnosticReporter(diagnostics));
        var lowering = new Lowering();

        var syntaxTrees = new List<SyntaxTree>();
        var project = Project.Load(options.Path);
        foreach (var sourceFile in project.SourceFiles)
        {
            var lexerOptions = new LexerOptions(
                new LexerDiagnosticReporter(diagnostics, sourceFile));
            var parserOptions = new ParserOptions(
                sourceFile,
                new ParserDiagnosticReporter(diagnostics, sourceFile));

            var code = File.ReadAllText(sourceFile.FilePath);
            var tokens = lexer.Tokenize(code, lexerOptions);
            var tree = parser.Parse(tokens, parserOptions);

            syntaxTrees.Add(tree);
        }

        var semanticResult = semantic.Analyze(syntaxTrees, semanticOptions);

        if (diagnostics.Diagnostics.Count > 0)
        {
            foreach (var diagnostic in diagnostics.Diagnostics)
            {
                var (start, end) = diagnostic.Location.Span;
                var span = start != end
                    ? $"{start.Line}:{start.Column} - {end.Line}:{end.Column}"
                    : $"{start.Line}:{start.Column}";

                // $"{diagnostic.File}({span}): {diagnostic.Severity} {diagnostic.Id} - {diagnostic.Message}"
                var originalColor = Console.ForegroundColor;

                Console.ForegroundColor = ConsoleColor.Blue;
                Console.Write($"{diagnostic.Location.File}({span})");

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

            return;
        }

        foreach (var semanticTree in semanticResult.SemanticTrees)
            lowering.Lower(
                semanticTree,
                new LoweringOptions(options.Directives, semanticResult.ControlFlowGraphs));

        var ir = new IrGenerator();
        var functions = ir.Generate(
            semanticResult.MetadataProvider.Types,
            semanticResult.SemanticTrees);

        if (options.PrintIr)
            foreach (var function in functions)
                Console.WriteLine(function);

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