using Trilang.Compilation;
using Trilang.Compilation.Diagnostics;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class CompoundAssignmentTargetValidation : ISemanticPass
{
    private readonly ISet<string> directives;
    private readonly SemanticDiagnosticReporter diagnostics;

    public CompoundAssignmentTargetValidation(ISet<string> directives, DiagnosticCollection diagnostics)
    {
        this.directives = directives;
        this.diagnostics = diagnostics.ForSemantic();
    }

    public void Analyze(Project project)
    {
        var visitor = new CompoundAssignmentTargetValidationVisitor(directives, diagnostics);
        var semanticTrees = project.SourceFiles.Select(x => x.SemanticTree!);
        foreach (var tree in semanticTrees)
            tree.Accept(visitor);
    }

    public string Name => nameof(CompoundAssignmentTargetValidation);

    public IEnumerable<string> DependsOn => [];

    private sealed class CompoundAssignmentTargetValidationVisitor : Visitor
    {
        private readonly SemanticDiagnosticReporter diagnostics;

        public CompoundAssignmentTargetValidationVisitor(ISet<string> directives, SemanticDiagnosticReporter diagnostics)
            : base(directives)
        {
            this.diagnostics = diagnostics;
        }

        public override void VisitBinaryExpression(BinaryExpression node)
        {
            base.VisitBinaryExpression(node);

            if (!node.Kind.IsArithmeticCompoundAssignment() && !node.Kind.IsBitwiseCompoundAssignment())
                return;

            if (node.Left is not MemberAccessExpression and not ArrayAccessExpression)
                diagnostics.InvalidOperandForCompoundAssignment(node);
        }
    }
}