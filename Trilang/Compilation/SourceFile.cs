namespace Trilang.Compilation;

public class SourceFile
{
    public SourceFile(string filePath, string name)
    {
        FilePath = filePath;
        Name = name;
    }

    public string FilePath { get; }

    public string Name { get; }
}