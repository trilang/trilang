using System.Text.Json;
using Trilang.Compilation.Diagnostics;

namespace Trilang.Compilation;

internal class ProjectLoader
{
    private static readonly JsonSerializerOptions jsonOptions;
    private readonly CompilerDiagnosticReporter diagnostics;

    static ProjectLoader()
    {
        jsonOptions = new JsonSerializerOptions(JsonSerializerOptions.Default)
        {
            PropertyNameCaseInsensitive = true,
        };
    }

    public ProjectLoader(DiagnosticCollection diagnostics)
        => this.diagnostics = diagnostics.ForCompiler();

    public IEnumerable<ProjectInfo> LoadInfo(string path)
    {
        var projects = new Dictionary<string, ProjectInfo>();
        var project = Load(projects, path);

        return SortProjects(project);
    }

    private ProjectInfo Load(Dictionary<string, ProjectInfo> projects, string path)
    {
        // TODO: resolve links
        if (projects.TryGetValue(path, out var project))
            return project;

        var json = LoadJson(path);

        project = new ProjectInfo(json.Name, path);
        projects.Add(path, project);

        foreach (var dependencyPath in json.Dependencies)
        {
            var dependency = Load(projects, dependencyPath);
            if (!project.AddDependency(dependency))
                diagnostics.DuplicateDependency(project, dependency);
        }

        return project;
    }

    private List<ProjectInfo> SortProjects(ProjectInfo root)
    {
        var allProjects = CollectProjects(root);
        var inDegree = new Dictionary<ProjectInfo, int>();
        var dependents = new Dictionary<ProjectInfo, List<ProjectInfo>>();

        foreach (var project in allProjects)
        {
            inDegree[project] = 0;
            dependents[project] = [];
        }

        foreach (var project in allProjects)
        {
            foreach (var dep in project.Dependencies)
            {
                inDegree[project]++;
                dependents[dep].Add(project);
            }
        }

        var queue = new Queue<ProjectInfo>();
        var result = new List<ProjectInfo>();

        foreach (var (key, value) in inDegree)
            if (value == 0)
                queue.Enqueue(key);

        while (queue.TryDequeue(out var current))
        {
            result.Add(current);

            foreach (var dependent in dependents[current])
            {
                inDegree[dependent]--;

                if (inDegree[dependent] == 0)
                    queue.Enqueue(dependent);
            }
        }

        if (result.Count != allProjects.Count)
            diagnostics.CyclicDependency();

        return result;
    }

    private HashSet<ProjectInfo> CollectProjects(ProjectInfo root)
    {
        var result = new HashSet<ProjectInfo>(ProjectInfoComparer.Instance);
        var stack = new Stack<ProjectInfo>();
        stack.Push(root);

        while (stack.TryPop(out var current))
        {
            if (!result.Add(current))
            {
                diagnostics.DuplicateDependency(current);
                break;
            }

            foreach (var dep in current.Dependencies)
                stack.Push(dep);
        }

        return result;
    }

    private ProjectJson LoadJson(string path)
    {
        var ext = Path.GetExtension(path);
        if (ext == ".tri")
        {
            var name = Path.GetFileNameWithoutExtension(path);

            return new ProjectJson { Name = name };
        }

        if (ext == ".project")
        {
            var projectFile = File.ReadAllText(path);
            var projectJson = JsonSerializer.Deserialize<ProjectJson>(projectFile, jsonOptions) ??
                              throw new InvalidOperationException("Unable to deserialize the project file.");

            return projectJson;
        }

        throw new InvalidOperationException("Unknown project file");
    }

    public Project LoadProject(ProjectInfo projectInfo)
    {
        var path = projectInfo.Path;
        var ext = Path.GetExtension(path);
        var compilationUnits = ext switch
        {
            ".tri" => [new CompilationUnit(new SourceFile(path, File.ReadAllText(path)))],

            ".project" => Directory
                .GetFiles(Path.GetDirectoryName(path)!, "*.tri", SearchOption.AllDirectories)
                .Select(file => new CompilationUnit(new SourceFile(file, File.ReadAllText(file)))),

            _ => throw new InvalidOperationException("Unknown project file"),
        };

        return new Project(projectInfo.Name, projectInfo.Path, compilationUnits.ToList(), projectInfo.Dependencies);
    }

    private sealed class ProjectJson
    {
        public required string Name { get; set; }

        public List<string> Dependencies { get; set; } = [];
    }

    private sealed class ProjectInfoComparer : IEqualityComparer<ProjectInfo>
    {
        public static readonly ProjectInfoComparer Instance = new ProjectInfoComparer();

        public bool Equals(ProjectInfo? x, ProjectInfo? y)
        {
            if (ReferenceEquals(x, y))
                return true;

            if (x is null)
                return false;

            if (y is null)
                return false;

            if (x.GetType() != y.GetType())
                return false;

            return x.Name == y.Name;
        }

        public int GetHashCode(ProjectInfo obj)
            => obj.Name.GetHashCode();
    }
}