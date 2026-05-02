using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class CheckStaticAndInstanceMembersAccess : ISemanticPass
{
    private readonly ISet<string> directives;
    private readonly SemanticDiagnosticReporter diagnostics;

    public CheckStaticAndInstanceMembersAccess(
        ISet<string> directives,
        DiagnosticCollection diagnostics)
    {
        this.directives = directives;
        this.diagnostics = diagnostics.ForSemantic();
    }

    public void Analyze(IEnumerable<SemanticTree> semanticTrees)
    {
        foreach (var tree in semanticTrees)
            tree.Accept(new CheckStaticAndInstanceMembersAccessVisitor(directives, diagnostics));
    }

    public string Name => nameof(CheckStaticAndInstanceMembersAccess);

    public IEnumerable<string> DependsOn => [nameof(TypeChecker)];

    private sealed class CheckStaticAndInstanceMembersAccessVisitor : Visitor
    {
        private readonly SemanticDiagnosticReporter diagnostics;

        public CheckStaticAndInstanceMembersAccessVisitor(
            ISet<string> directives,
            SemanticDiagnosticReporter diagnostics)
            : base(directives)
        {
            this.diagnostics = diagnostics;
        }

        public override void VisitMemberAccess(MemberAccessExpression node)
        {
            base.VisitMemberAccess(node);

            if (node.Member is not MemberAccessExpression parent)
                return;

            if (node.Reference is null)
                return;

            var parentRef = parent.Reference;
            if (parentRef is null)
                return;

            if (parentRef is ITypeMetadata type)
            {
                if (type.IsInvalid)
                    return;

                if (type.UnpackAlias() is TypeMetadata)
                {
                    if (node.Reference is MethodMetadata { IsStatic: false })
                        diagnostics.InstanceMethodAsStatic(node);
                }
            }
            else
            {
                if (node.Reference is MethodMetadata { IsStatic: true })
                    diagnostics.StaticMethodAsInstance(node);
            }
        }
    }
}