using Trilang.Compilation.Diagnostics;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class BreakContinueWithinLoop : Visitor, ISemanticPass
{
    private readonly SemanticDiagnosticReporter diagnostics;

    public BreakContinueWithinLoop(ISet<string> directives, SemanticDiagnosticReporter diagnostics)
        : base(directives)
    {
        this.diagnostics = diagnostics;
    }

    public void Analyze(IEnumerable<SemanticTree> semanticTrees)
    {
        foreach (var tree in semanticTrees)
            tree.Accept(this);
    }

    public override void VisitBreak(Break node)
    {
        var loop = node.FindInParent<While>();
        if (loop is null)
            diagnostics.BreakOutsideLoop(node);

        node.LoopNode = loop;

        base.VisitBreak(node);
    }

    public override void VisitContinue(Continue node)
    {
        var loop = node.FindInParent<While>();
        if (loop is null)
            diagnostics.ContinueOutsideLoop(node);

        node.LoopNode = loop;

        base.VisitContinue(node);
    }

    public string Name => nameof(BreakContinueWithinLoop);

    public IEnumerable<string> DependsOn => [];
}