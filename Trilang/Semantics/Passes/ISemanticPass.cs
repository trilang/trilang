using Trilang.Compilation;

namespace Trilang.Semantics.Passes;

internal interface ISemanticPass
{
    string Name { get; }

    IEnumerable<string> DependsOn { get; }

    void Analyze(Project project);
}