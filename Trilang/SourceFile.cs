namespace Trilang;

public record SourceFile(string FilePath)
{
    public override string ToString()
        => Path.GetFileName(FilePath);
}