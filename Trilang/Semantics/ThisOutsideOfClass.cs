using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Semantics;

internal class ThisOutsideOfClass : Visitor
{
    protected override void VisitEnter(MemberAccessExpressionNode node)
    {
        if (!node.IsThis)
            return;

        var ctor = node.FindInParent<ConstructorDeclarationNode>();
        if (ctor is not null)
            return;

        var method = node.FindInParent<MethodDeclarationNode>();
        if (method is not null)
            return;

        throw new SemanticAnalysisException("The 'this' keyword is only allowed inside a class.");
    }
}