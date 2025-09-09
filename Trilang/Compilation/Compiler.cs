using Trilang.IntermediateRepresentation;
using Trilang.Lower;
using Trilang.Metadata;
using Trilang.OutputFormats.Elf;
using Trilang.Parsing;
using Trilang.Semantics;
using Trilang.Semantics.Model;

namespace Trilang.Compilation;

public class Compiler
{
    public void Compile(CompilerOptions options)
    {
        var parser = new Parser();
        var rootTypeMetadataProvider = new RootTypeMetadataProvider();
        var semantic = new SemanticAnalysis(rootTypeMetadataProvider);
        var semanticOptions = new SemanticAnalysisOptions(options.Directives);
        var lowering = new Lowering();

        var semanticTrees = new List<SemanticTree>();
        var project = Project.Load(options.Path);
        foreach (var sourceFile in project.SourceFiles)
        {
            var code = File.ReadAllText(sourceFile.FilePath);
            var tree = parser.Parse(code);
            var (semanticTree, _, _, cfgs) = semantic.Analyze(tree, semanticOptions);
            lowering.Lower(semanticTree, new LoweringOptions(options.Directives, cfgs));

            semanticTrees.Add(semanticTree);
        }

        var ir = new IrGenerator();
        var functions = ir.Generate(rootTypeMetadataProvider.Types, semanticTrees);
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