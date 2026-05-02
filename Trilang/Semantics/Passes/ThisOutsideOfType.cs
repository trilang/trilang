using Trilang.Compilation;
using Trilang.Compilation.Diagnostics;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class ThisOutsideOfType : ISemanticPass
{
    private readonly ISet<string> directives;
    private readonly SemanticDiagnosticReporter diagnostics;

    public ThisOutsideOfType(ISet<string> directives, DiagnosticCollection diagnostics)
    {
        this.directives = directives;
        this.diagnostics = diagnostics.ForSemantic();
    }

    public void Analyze(Project project)
    {
        var semanticTrees = project.SourceFiles.Select(x => x.SemanticTree!);
        var visitor = new ThisOutsideOfTypeVisitor(directives, diagnostics);
        foreach (var tree in semanticTrees)
            tree.Accept(visitor);
    }

    public string Name => nameof(ThisOutsideOfType);

    public IEnumerable<string> DependsOn => [];

    private sealed class ThisOutsideOfTypeVisitor : Visitor
    {
        private readonly SemanticDiagnosticReporter diagnostics;

        public ThisOutsideOfTypeVisitor(
            ISet<string> directives,
            SemanticDiagnosticReporter diagnostics)
            : base(directives)
        {
            this.diagnostics = diagnostics;
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
    }
}