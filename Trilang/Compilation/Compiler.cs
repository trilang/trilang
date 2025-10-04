using Trilang.Compilation.Diagnostics;
using Trilang.IntermediateRepresentation;
using Trilang.Lexing;
using Trilang.Lower;
using Trilang.Metadata;
using Trilang.OutputFormats.Elf;
using Trilang.Parsing;
using Trilang.Semantics;

namespace Trilang.Compilation;

public class Compiler
{
    public void Compile(CompilerOptions options)
    {
        var diagnostics = new DiagnosticCollection();
        var lexerOptions = new LexerOptions(diagnostics.Lexer);
        var parserOptions = new ParserOptions(diagnostics.Parser);

        var lexer = new Lexer();
        var parser = new Parser();
        var rootTypeMetadataProvider = new RootTypeMetadataProvider();
        var semantic = new SemanticAnalysis(rootTypeMetadataProvider);
        var semanticOptions = new SemanticAnalysisOptions(options.Directives);
        var lowering = new Lowering();

        var semanticResults = new List<SemanticAnalysisResult>();
        var project = Project.Load(options.Path);
        foreach (var sourceFile in project.SourceFiles)
        {
            diagnostics.SwitchFile(sourceFile);

            var code = File.ReadAllText(sourceFile.FilePath);
            var tokens = lexer.Tokenize(code, lexerOptions);
            var tree = parser.Parse(tokens, parserOptions);
            var semanticResult = semantic.Analyze(tree, semanticOptions);

            semanticResults.Add(semanticResult);
        }

        if (diagnostics.Diagnostics.Count > 0)
        {
            foreach (var diagnostic in diagnostics.Diagnostics)
            {
                var (start, end) = diagnostic.SourceSpan;
                var span = start != end
                    ? $"{start.Line}:{start.Column} - {end.Line}:{end.Column}"
                    : $"{start.Line}:{start.Column}";

                // $"{diagnostic.File}({span}): {diagnostic.Severity} {diagnostic.Id} - {diagnostic.Message}"
                var originalColor = Console.ForegroundColor;

                Console.ForegroundColor = ConsoleColor.Blue;
                Console.Write($"{diagnostic.File}({span})");

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

        foreach (var (semanticTree, _, _, cfgs) in semanticResults)
            lowering.Lower(semanticTree, new LoweringOptions(options.Directives, cfgs));

        var ir = new IrGenerator();
        var functions = ir.Generate(
            rootTypeMetadataProvider.Types,
            semanticResults.Select(x => x.SemanticTree));

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