using System.Text.Json;

namespace Trilang.Compilation;

public class Project
{
    private readonly List<SourceFile> sourceFiles;

    private Project(string projectPath, IEnumerable<SourceFile> files)
    {
        ProjectPath = projectPath;
        sourceFiles = [.. files];
    }

    public static Project Load(string path)
    {
        var name = string.Empty;
        var files = new List<SourceFile>();

        var ext = Path.GetExtension(path);
        if (ext == ".tri")
        {
            name = Path.GetFileNameWithoutExtension(path);
            files.Add(new SourceFile(path));
        }
        else if (ext == ".project")
        {
            var projectFile = File.ReadAllText(path);
            var jsonOptions = new JsonSerializerOptions(JsonSerializerOptions.Default)
            {
                PropertyNameCaseInsensitive = true,
            };

            var projectJson = JsonSerializer.Deserialize<ProjectJson>(projectFile, jsonOptions) ??
                              throw new Exception();

            name = projectJson.Name;
            files.AddRange(Directory
                .GetFiles(Path.GetDirectoryName(path)!, "*.tri", SearchOption.AllDirectories)
                .Select(file => new SourceFile(file)));
        }
        else
        {
            throw new Exception();
        }

        return new Project(name, files);
    }

    public string ProjectPath { get; }

    public IReadOnlyList<SourceFile> SourceFiles => sourceFiles;

    private sealed class ProjectJson
    {
        public string Name { get; set; } = string.Empty;
    }
}