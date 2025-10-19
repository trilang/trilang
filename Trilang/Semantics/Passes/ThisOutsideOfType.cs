using Trilang.Compilation.Diagnostics;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class ThisOutsideOfType : Visitor, ISemanticPass
{
    private SemanticDiagnosticReporter diagnostics = null!;

    public void Analyze(SemanticTree tree, SemanticPassContext context)
    {
        diagnostics = context.Diagnostics;

        tree.Accept(this);
    }

    protected override void VisitMemberAccessEnter(MemberAccessExpression node)
    {
        if (!node.IsThis)
            return;

        var type = node.FindInParent<TypeDeclaration>();
        if (type is not null)
            return;

        diagnostics.ThisOutsideOfType(node);
    }

    public string Name => nameof(ThisOutsideOfType);

    public IEnumerable<string> DependsOn => [];
}