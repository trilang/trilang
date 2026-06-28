using Trilang.Compilation;
using Trilang.Compilation.Diagnostics;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class VariableDuplicate : ISemanticPass
{
    private readonly ISet<string> directives;
    private readonly SemanticDiagnosticReporter diagnostics;

    public VariableDuplicate(ISet<string> directives, DiagnosticCollection diagnostics)
    {
        this.directives = directives;
        this.diagnostics = diagnostics.ForSemantic();
    }

    public void Analyze(Project project)
    {
        var semanticTrees = project.SourceFiles.Select(x => x.SemanticTree!);
        var visitor = new VariableDuplicateVisitor(directives, diagnostics);
        foreach (var tree in semanticTrees)
            tree.Accept(visitor);
    }

    public string Name => nameof(VariableDuplicate);

    public IEnumerable<string> DependsOn => [nameof(MetadataGenerator)];

    private sealed class VariableDuplicateVisitor : Visitor
    {
        private readonly SemanticDiagnosticReporter diagnostics;

        public VariableDuplicateVisitor(ISet<string> directives, SemanticDiagnosticReporter diagnostics)
            : base(directives)
        {
            this.diagnostics = diagnostics;
        }

        public override void VisitBlock(BlockStatement node)
        {
            var variables = new Dictionary<string, List<VariableDeclaration>>();

            foreach (var variable in node.Statements.OfType<VariableDeclaration>())
            {
                if (!variables.TryGetValue(variable.Name, out var list))
                {
                    list = [];
                    variables[variable.Name] = list;
                }

                list.Add(variable);
            }

            foreach (var group in variables.Values.Where(g => g.Count > 1))
            foreach (var variable in group)
            {
                variable.Metadata!.MarkAsInvalid();
                diagnostics.VariableAlreadyDefined(variable);
            }

            base.VisitBlock(node);
        }
    }
}