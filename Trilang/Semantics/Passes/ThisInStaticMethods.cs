using Trilang.Compilation.Diagnostics;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class ThisInStaticMethods : Visitor, ISemanticPass
{
    private readonly SemanticDiagnosticReporter diagnostics;

    public ThisInStaticMethods(ISet<string> directives, SemanticDiagnosticReporter diagnostics)
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
        if (!node.IsThis)
            return;

        var method = node.FindInParent<MethodDeclaration>();
        if (method is null || !method.IsStatic)
            return;

        diagnostics.ThisInStaticMethod(node);

        base.VisitMemberAccess(node);
    }

    public string Name => nameof(ThisInStaticMethods);

    public IEnumerable<string> DependsOn => [];
}