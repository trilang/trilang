namespace Trilang.Compilation;

public record Project(
    string Name,
    string Path,
    IReadOnlyCollection<CompilationUnit> SourceFiles,
    IReadOnlyCollection<ProjectInfo> Dependencies)
{
    public ProjectInfo ToProjectInfo()
        => new ProjectInfo(Name, Path);
}