using Trilang.Metadata;
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

        if (node.Reference is not PropertyMetadata and not MethodMetadata)
            return;

        var parent = node.FindInParent<TypeDeclarationNode>()!;

        node.Member = new MemberAccessExpressionNode(MemberAccessExpressionNode.This)
        {
            Reference = parent.Metadata,
            AccessKind = PropertyAccessKind.Read,
        };
    }
}