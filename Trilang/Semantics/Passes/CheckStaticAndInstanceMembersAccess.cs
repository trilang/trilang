using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class CheckStaticAndInstanceMembersAccess : Visitor, ISemanticPass
{
    private SemanticDiagnosticReporter diagnostics = null!;

    public void Analyze(SemanticTree tree, SemanticPassContext context)
    {
        diagnostics = context.Diagnostics;

        tree.Accept(this);
    }

    protected override void VisitMemberAccessExit(MemberAccessExpression node)
    {
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

    public string Name => nameof(CheckStaticAndInstanceMembersAccess);

    public IEnumerable<string> DependsOn => [nameof(TypeChecker)];
}