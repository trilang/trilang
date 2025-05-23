using Trilang.OutputFormats.Elf;
using Trilang.Parsing;
using Trilang.Semantics;

namespace Trilang.Compilation;

public class Compiler
{
    public void Compile(CompilerOptions options)
    {
        var parser = new Parser();
        var semantic = new SemanticAnalysis();

        var project = Project.Load(options.Path);
        foreach (var sourceFile in project.SourceFiles)
        {
            var code = File.ReadAllText(sourceFile.FilePath);
            var tree = parser.Parse(code);
            semantic.Analyze(tree);
        }

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