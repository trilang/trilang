using CommandLine;
using Trilang.Compilation;

namespace Tri;

internal class Options
{
    [Option('o', "output", Required = true, HelpText = "Specifies the output path for the compiled binary.")]
    public required string OutputPath { get; set; }

    [Option('p', "path", Required = false, HelpText = "Specifies the path to the Tri project file to compile.")]
    public string? Path { get; set; }

    [Option("os", Required = false, HelpText = "Specifies the target operating system for compilation.")]
    public CompilerOptionOs? OperatingSystem { get; set; }

    [Option("platform", Required = false, HelpText = "Specifies the target platform architecture for compilation.")]
    public CompilerOptionPlatform? Platform { get; set; }

    [Option('d', "directive", Required = false, Separator = ',', HelpText = "Specifies a directive to include in the compilation.")]
    public IEnumerable<string>? Directives { get; set; }
}