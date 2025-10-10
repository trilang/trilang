namespace Trilang;

public readonly record struct SourceFile(string FilePath)
{
    public override string ToString()
        => Path.GetFileName(FilePath);
}