using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Lower;

internal class AddThisInLocalMemberAccess : Visitor
{
    protected override void VisitMemberAccessExit(MemberAccessExpressionNode node)
    {
        if (node.Member is not null)
            return;

        if (node.IsThis || node.IsField || node.IsValue)
            return;

        if (node.Reference is not PropertyDeclarationNode and not MethodDeclarationNode)
            return;

        var parent = node.FindInParent<TypeDeclarationNode>()!;

        node.Member = new MemberAccessExpressionNode(MemberAccessExpressionNode.This)
        {
            ReturnTypeMetadata = parent.Metadata,
            Reference = parent,
        };
    }
}