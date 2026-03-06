using Trilang.Compilation.Diagnostics;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class ThisOutsideOfType : Visitor, ISemanticPass
{
    private readonly SemanticDiagnosticReporter diagnostics;

    public ThisOutsideOfType(ISet<string> directives, SemanticDiagnosticReporter diagnostics)
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

        var type = node.FindInParent<TypeDeclaration>();
        if (type is not null)
            return;

        diagnostics.ThisOutsideOfType(node);

        base.VisitMemberAccess(node);
    }

    public string Name => nameof(ThisOutsideOfType);

    public IEnumerable<string> DependsOn => [];
}