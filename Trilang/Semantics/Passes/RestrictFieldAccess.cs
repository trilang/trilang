using Trilang.Compilation;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class RestrictFieldAccess : ISemanticPass
{
    private readonly ISet<string> directives;
    private readonly SemanticDiagnosticReporter diagnostics;

    public RestrictFieldAccess(ISet<string> directives, DiagnosticCollection diagnostics)
    {
        this.directives = directives;
        this.diagnostics = diagnostics.ForSemantic();
    }

    public void Analyze(Project project)
    {
        var semanticTrees = project.SourceFiles.Select(x => x.SemanticTree!);
        var visitor = new RestrictFieldAccessVisitor(directives, diagnostics);
        foreach (var tree in semanticTrees)
            tree.Accept(visitor);
    }

    public string Name => nameof(RestrictFieldAccess);

    public IEnumerable<string> DependsOn => [nameof(TypeChecker)];

    private sealed class RestrictFieldAccessVisitor : Visitor
    {
        private readonly SemanticDiagnosticReporter diagnostics;

        public RestrictFieldAccessVisitor(
            ISet<string> directives,
            SemanticDiagnosticReporter diagnostics)
            : base(directives)
        {
            this.diagnostics = diagnostics;
        }

        public override void VisitMemberAccess(MemberAccessExpression node)
        {
            base.VisitMemberAccess(node);

            if (node.IsFirstMember)
                return;

            if (node.Reference is FieldMetadata)
                diagnostics.FieldNotAccessible(node);
        }
    }
}