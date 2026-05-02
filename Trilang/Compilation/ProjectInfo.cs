namespace Trilang.Compilation;

public class ProjectInfo
{
    private readonly HashSet<ProjectInfo> dependencies;

    public ProjectInfo(string name, string path)
    {
        dependencies = [];

        Name = name;
        Path = path;
    }

    public bool AddDependency(ProjectInfo dependency)
        => dependencies.Add(dependency);

    public string Name { get; }

    public string Path { get; }

    public IReadOnlyCollection<ProjectInfo> Dependencies
        => dependencies;
}