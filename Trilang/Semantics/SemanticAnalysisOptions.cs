namespace Trilang.Semantics;

public record SemanticAnalysisOptions(IEnumerable<string> Directives)
{
    public static readonly SemanticAnalysisOptions Default = new SemanticAnalysisOptions([]);

    public bool HasDirective(string name)
        => Directives.Contains(name);
}