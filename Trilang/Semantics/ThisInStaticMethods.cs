using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Semantics;

internal class ThisInStaticMethods : Visitor
{
    protected override void VisitEnter(MemberAccessExpressionNode node)
    {
        if (!node.IsThis)
            return;

        var method = node.FindInParent<MethodDeclarationNode>();
        if (method is null || !method.IsStatic)
            return;

        throw new SemanticAnalysisException("The 'this' keyword is not allowed in static methods.");
    }
}