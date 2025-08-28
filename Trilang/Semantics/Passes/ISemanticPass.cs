using Trilang.Parsing.Ast;

namespace Trilang.Semantics.Passes;

internal interface ISemanticPass
{
    string Name { get; }
    IEnumerable<string> DependsOn { get; }

    void Analyze(SyntaxTree tree, SemanticPassContext context);
}