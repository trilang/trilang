using Trilang.Compilation.Diagnostics;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class BreakContinueWithinLoop : Visitor, ISemanticPass
{
    private SemanticDiagnosticReporter diagnostics = null!;

    public void Analyze(SemanticTree tree, SemanticPassContext context)
    {
        diagnostics = context.Diagnostics;

        tree.Accept(this);
    }

    protected override void VisitBreakEnter(Break node)
    {
        var loop = node.FindInParent<While>();
        if (loop is null)
            diagnostics.BreakOutsideLoop(node);

        node.LoopNode = loop;
    }

    protected override void VisitContinueEnter(Continue node)
    {
        var loop = node.FindInParent<While>();
        if (loop is null)
            diagnostics.ContinueOutsideLoop(node);

        node.LoopNode = loop;
    }

    public string Name => nameof(BreakContinueWithinLoop);

    public IEnumerable<string> DependsOn => [];
}