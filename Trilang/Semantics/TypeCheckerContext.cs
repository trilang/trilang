namespace Trilang.Semantics;

public record TypeCheckerContext(IEnumerable<string> Directives)
{
    public bool HasDirective(string name)
        => Directives.Contains(name);
}