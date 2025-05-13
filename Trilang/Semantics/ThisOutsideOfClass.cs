using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Semantics;

internal class ThisOutsideOfClass : Visitor
{
    protected override void VisitEnter(MemberAccessExpressionNode node)
    {
        if (!node.IsThis)
            return;

        var type = node.FindInParent<TypeDeclarationNode>();
        if (type is not null)
            return;

        throw new SemanticAnalysisException("The 'this' keyword is only allowed inside a type.");
    }
}