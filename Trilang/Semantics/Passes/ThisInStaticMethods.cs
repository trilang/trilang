using Trilang.Compilation;
using Trilang.Compilation.Diagnostics;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class ThisInStaticMethods : ISemanticPass
{
    private readonly ISet<string> directives;
    private readonly SemanticDiagnosticReporter diagnostics;

    public ThisInStaticMethods(ISet<string> directives, DiagnosticCollection diagnostics)
    {
        this.directives = directives;
        this.diagnostics = diagnostics.ForSemantic();
    }

    public void Analyze(Project project)
    {
        var semanticTrees = project.SourceFiles.Select(x => x.SemanticTree!);
        var visitor = new ThisInStaticMethodsVisitor(directives, diagnostics);
        foreach (var tree in semanticTrees)
            tree.Accept(visitor);
    }

    public string Name => nameof(ThisInStaticMethods);

    public IEnumerable<string> DependsOn => [];

    private sealed class ThisInStaticMethodsVisitor : Visitor
    {
        private readonly SemanticDiagnosticReporter diagnostics;

        public ThisInStaticMethodsVisitor(
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

            var method = node.FindInParent<MethodDeclaration>();
            if (method is null || !method.IsStatic)
                return;

            diagnostics.ThisInStaticMethod(node);

            base.VisitMemberAccess(node);
        }
    }
}