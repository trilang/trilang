using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal interface ISemanticPass
{
    string Name { get; }

    IEnumerable<string> DependsOn { get; }

    void Analyze(IEnumerable<SemanticTree> semanticTrees, SemanticPassContext context);
}