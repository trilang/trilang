using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class RestrictFieldAccess : Visitor, ISemanticPass
{
    private SemanticDiagnosticReporter diagnostics = null!;

    public void Analyze(SemanticTree tree, SemanticPassContext context)
    {
        diagnostics = context.Diagnostics;

        tree.Accept(this);
    }

    protected override void VisitMemberAccessExit(MemberAccessExpression node)
    {
        if (node.IsFirstMember)
            return;

        if (node.Reference is FieldMetadata)
            diagnostics.FieldNotAccessible(node);
    }

    public string Name => nameof(RestrictFieldAccess);

    public IEnumerable<string> DependsOn => [nameof(TypeChecker)];
}