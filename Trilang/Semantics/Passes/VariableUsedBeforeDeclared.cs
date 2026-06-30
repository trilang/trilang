using Trilang.Compilation;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class VariableUsedBeforeDeclared : ISemanticPass
{
    private readonly ISet<string> directives;
    private readonly SemanticDiagnosticReporter diagnostics;

    public VariableUsedBeforeDeclared(ISet<string> directives, DiagnosticCollection diagnostics)
    {
        this.directives = directives;
        this.diagnostics = diagnostics.ForSemantic();
    }

    public void Analyze(Project project)
    {
        var semanticTrees = project.SourceFiles.Select(x => x.SemanticTree!);
        var visitor = new VariableUsedBeforeDeclaredVisitor(directives, diagnostics);
        foreach (var tree in semanticTrees)
            tree.Accept(visitor);
    }

    public string Name => nameof(VariableUsedBeforeDeclared);

    public IEnumerable<string> DependsOn => [nameof(Binder), nameof(MetadataGenerator)];

    private sealed class VariableUsedBeforeDeclaredVisitor : Visitor
    {
        private readonly List<HashSet<string>> scopes;
        private readonly SemanticDiagnosticReporter diagnostics;

        public VariableUsedBeforeDeclaredVisitor(
            ISet<string> directives,
            SemanticDiagnosticReporter diagnostics)
            : base(directives)
        {
            this.diagnostics = diagnostics;
            scopes = [];
        }

        public override void VisitBlock(BlockStatement node)
        {
            scopes.Add([]);

            base.VisitBlock(node);

            scopes.RemoveAt(scopes.Count - 1);
        }

        public override void VisitVariable(VariableDeclaration node)
        {
            scopes[^1].Add(node.Name);

            base.VisitVariable(node);
        }

        public override void VisitMemberAccess(MemberAccessExpression node)
        {
            if (!node.IsFirstMember || node.IsThis || node.IsField || node.IsValue)
                return;

            if (node.Reference is not VariableMetadata)
                return;

            for (var i = scopes.Count - 1; i >= 0; i--)
                if (scopes[i].Contains(node.Name))
                    return;

            diagnostics.VariableUsedBeforeDeclaration(node);

            base.VisitMemberAccess(node);
        }
    }
}