using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class UnresolvedMemberAccess : ISemanticPass
{
    private readonly ISet<string> directives;
    private readonly SemanticDiagnosticReporter diagnostics;

    public UnresolvedMemberAccess(ISet<string> directives, DiagnosticCollection diagnostics)
    {
        this.directives = directives;
        this.diagnostics = diagnostics.ForSemantic();
    }

    public void Analyze(IEnumerable<SemanticTree> semanticTrees)
    {
        var visitor = new UnresolvedMemberAccessVisitor(directives, diagnostics);
        foreach (var tree in semanticTrees)
            tree.Accept(visitor);
    }

    public string Name
        => nameof(UnresolvedMemberAccess);

    public IEnumerable<string> DependsOn
        => [nameof(TypeChecker)];

    private sealed class UnresolvedMemberAccessVisitor : Visitor
    {
        private readonly SemanticDiagnosticReporter diagnostics;

        public UnresolvedMemberAccessVisitor(
            ISet<string> directives,
            SemanticDiagnosticReporter diagnostics)
            : base(directives)
        {
            this.diagnostics = diagnostics;
        }

        public override void VisitMemberAccess(MemberAccessExpression node)
        {
            base.VisitMemberAccess(node);

            if (node is
                {
                    Reference: null,
                    IsFirstMember: true,
                    IsThis: false,
                    IsField: false,
                    IsValue: false
                })
            {
                diagnostics.UnknownSymbol(node);
                return;
            }

            if (node.Reference is AggregateMetadata aggregate)
                diagnostics.MultipleMembersFound(node, aggregate.Members);
        }
    }
}