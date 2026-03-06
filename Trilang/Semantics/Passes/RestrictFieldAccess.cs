using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class RestrictFieldAccess : Visitor, ISemanticPass
{
    private readonly SemanticDiagnosticReporter diagnostics;

    public RestrictFieldAccess(ISet<string> directives, SemanticDiagnosticReporter diagnostics)
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

        if (node.IsFirstMember)
            return;

        if (node.Reference is FieldMetadata)
            diagnostics.FieldNotAccessible(node);
    }

    public string Name => nameof(RestrictFieldAccess);

    public IEnumerable<string> DependsOn => [nameof(TypeChecker)];
}