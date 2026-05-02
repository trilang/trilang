using Trilang.Compilation;
using Trilang.Compilation.Diagnostics;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class BreakContinueWithinLoop : ISemanticPass
{
    private readonly ISet<string> directives;
    private readonly SemanticDiagnosticReporter diagnostics;

    public BreakContinueWithinLoop(ISet<string> directives, DiagnosticCollection diagnostics)
    {
        this.directives = directives;
        this.diagnostics = diagnostics.ForSemantic();
    }

    public void Analyze(Project project)
    {
        var semanticTrees = project.SourceFiles.Select(x => x.SemanticTree!);
        var visitor = new BreakContinueWithinLoopVisitor(directives, diagnostics);
        foreach (var tree in semanticTrees)
            tree.Accept(visitor);
    }

    public string Name => nameof(BreakContinueWithinLoop);

    public IEnumerable<string> DependsOn => [];

    private sealed class BreakContinueWithinLoopVisitor : Visitor
    {
        private readonly SemanticDiagnosticReporter diagnostics;

        public BreakContinueWithinLoopVisitor(
            ISet<string> directives,
            SemanticDiagnosticReporter diagnostics)
            : base(directives)
        {
            this.diagnostics = diagnostics;
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
    }
}