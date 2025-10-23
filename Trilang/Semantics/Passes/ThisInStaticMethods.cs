using Trilang.Compilation.Diagnostics;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class ThisInStaticMethods : Visitor, ISemanticPass
{
    private SemanticDiagnosticReporter diagnostics = null!;

    public void Analyze(IEnumerable<SemanticTree> semanticTrees, SemanticPassContext context)
    {
        diagnostics = context.Diagnostics;

        foreach (var tree in semanticTrees)
            tree.Accept(this);
    }

    protected override void VisitMemberAccessEnter(MemberAccessExpression node)
    {
        if (!node.IsThis)
            return;

        var method = node.FindInParent<MethodDeclaration>();
        if (method is null || !method.IsStatic)
            return;

        diagnostics.ThisInStaticMethod(node);
    }

    public string Name => nameof(ThisInStaticMethods);

    public IEnumerable<string> DependsOn => [];
}