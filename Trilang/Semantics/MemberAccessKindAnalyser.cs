using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Semantics;

internal class MemberAccessKindAnalyser : Visitor
{
    private static MemberAccessKind FindParentAssignment(MemberAccessExpressionNode node)
    {
        var parent = node.Parent;
        while (parent is not null)
        {
            if (parent is BinaryExpressionNode { Kind: BinaryExpressionKind.Assignment } assignment)
            {
                if (ReferenceEquals(assignment.Left, node))
                    return MemberAccessKind.Write;

                return MemberAccessKind.Read;
            }

            if (parent is BinaryExpressionNode { IsCompoundAssignment: true } compound)
            {
                if (ReferenceEquals(compound.Left, node))
                    return MemberAccessKind.ReadWrite;

                return MemberAccessKind.Read;
            }

            parent = parent.Parent;
        }

        return MemberAccessKind.Read;
    }

    protected override void VisitMemberAccessEnter(MemberAccessExpressionNode node)
        => node.AccessKind = FindParentAssignment(node);
}