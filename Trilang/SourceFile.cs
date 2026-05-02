namespace Trilang;

public record SourceFile(string FilePath, string Content)
{
    public override string ToString()
        => Path.GetFileName(FilePath);
}