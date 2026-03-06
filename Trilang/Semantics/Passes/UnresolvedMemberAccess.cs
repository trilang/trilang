using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class UnresolvedMemberAccess : Visitor, ISemanticPass
{
    private readonly SemanticDiagnosticReporter diagnostics;

    public UnresolvedMemberAccess(ISet<string> directives, SemanticDiagnosticReporter diagnostics)
        : base(directives)
    {
        this.diagnostics = diagnostics;
    }

    public void Analyze(IEnumerable<SemanticTree> semanticTrees)
    {
        foreach (var tree in semanticTrees)
            tree.Accept(this);
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

    public string Name
        => nameof(UnresolvedMemberAccess);

    public IEnumerable<string> DependsOn
        => [nameof(TypeChecker)];
}