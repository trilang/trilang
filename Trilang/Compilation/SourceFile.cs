namespace Trilang.Compilation;

public record SourceFile(string FilePath, string Name)
{
    public override string ToString()
        => Name;
}