using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class ThisInStaticMethods : Visitor, ISemanticPass
{
    public void Analyze(SemanticTree tree, SemanticPassContext context)
        => tree.Accept(this);

    protected override void VisitMemberAccessEnter(MemberAccessExpression node)
    {
        if (!node.IsThis)
            return;

        var method = node.FindInParent<MethodDeclaration>();
        if (method is null || !method.IsStatic)
            return;

        throw new SemanticAnalysisException("The 'this' keyword is not allowed in static methods.");
    }

    public string Name => nameof(ThisInStaticMethods);

    public IEnumerable<string> DependsOn => [];
}