using System.Runtime.InteropServices;

namespace Trilang.Compilation;

public class CompilerOptions
{
    public CompilerOptions(string outputPath)
    {
        OutputPath = outputPath;

        Path = Directory.GetCurrentDirectory();

        OperatingSystem = Environment.OSVersion.Platform switch
        {
            PlatformID.Unix => CompilerOptionOs.Linux,
            PlatformID.MacOSX => CompilerOptionOs.MacOs,

            _ => throw new ArgumentOutOfRangeException(),
        };

        Platform = RuntimeInformation.ProcessArchitecture switch
        {
            Architecture.X64 => CompilerOptionPlatform.X64,
            Architecture.Arm64 => CompilerOptionPlatform.Arm64,

            _ => throw new ArgumentOutOfRangeException(),
        };
    }

    public string OutputPath { get; }

    public string Path { get; set; }

    public CompilerOptionOs OperatingSystem { get; set; }

    public CompilerOptionPlatform Platform { get; set; }
}