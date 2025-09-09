using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal interface ISemanticPass
{
    string Name { get; }

    IEnumerable<string> DependsOn { get; }

    void Analyze(SemanticTree tree, SemanticPassContext context);
}