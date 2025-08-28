using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Semantics.Passes;

internal class ThisInStaticMethods : Visitor, ISemanticPass
{
    public void Analyze(SyntaxTree tree, SemanticPassContext context)
        => tree.Accept(this);

    protected override void VisitMemberAccessEnter(MemberAccessExpressionNode node)
    {
        if (!node.IsThis)
            return;

        var method = node.FindInParent<MethodDeclarationNode>();
        if (method is null || !method.IsStatic)
            return;

        throw new SemanticAnalysisException("The 'this' keyword is not allowed in static methods.");
    }

    public string Name => nameof(ThisInStaticMethods);

    public IEnumerable<string> DependsOn => [];
}